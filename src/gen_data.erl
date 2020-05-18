%%%-------------------------------------------------------------------
%%% @author ngq <ngq_scut@126.com>
%%% @doc
%%% 数据生成
%%% @end
%%%-------------------------------------------------------------------
-module(gen_data).

-export([main/1]).

-include("gen_data.hrl").
-include_lib("tools/include/excel.hrl").
-include_lib("tools/include/util.hrl").

-callback(init() -> {ok, State :: term()}).
-callback(translate(Sheets) -> DataInfo when
      Sheets :: [Sheet | OtherSheets],
      Sheet :: {SheetName :: binary(), RowInfo :: list()},
      OtherSheets :: term(),
      DataInfo :: map()).

-define(DEFAULT_ARGS, ["_build", "default"]).

main(reload) ->
    do_main(?DEFAULT_ARGS, reload);
main([]) ->
    main(?DEFAULT_ARGS);
main(Args) ->
    do_main(Args, escript).

do_main(Args, Mode) ->
    ?INFO("===========Start gen data!===========~n"),
    [begin
        try
            ?INFO("try gen data by ~p......~n", [Module]),
            {ok, #{
                source := InputPath,
                template := TemplatePath,
                data := OutputPath
            }} = Module:init(),
            ?INFO("init ok!~n"),
            {ok, Sheets} = lib_excel:open(InputPath),
            SheetInfo = data_info(Sheets, []),
            ?INFO("parse ok!~n"),
            DataInfo = Module:translate(SheetInfo),
            Template = bbmustache:parse_file(TemplatePath),
            Content = bbmustache:compile(Template, DataInfo),
            case Mode of
                reload ->
                    rl:load_from_source_bin(?LIST(Content));
                _ ->
                    file:write_file(OutputPath, Content)
            end,
            ?INFO("done!~n")
        catch
            E:R ->
                ?INFO("~p:~p~n", [E, R])
        end
    end || {Module, _} <- get_callback_mod(Args)],
    ?INFO("===========Finish gen data!===========~n"),
    ok.


%% 数据转换执行模块
get_callback_mod(Args) ->
    {ok, Path} = file:get_cwd(),
    AddPaths = rl:find_ebin_by_dir([filename:join([Path|Args])]),
    code:add_paths(AddPaths),
    L = rl:get_beam_file(AddPaths),
    lists:filter(fun is_callback_mod/1, L).

is_callback_mod({Module, _}) ->
    _Res = code:ensure_loaded(Module),
    Info = Module:module_info(attributes),
    lists:member({gen_data, [true]}, Info).


%% 文件信息
data_info([#excel_sheet{name = SheetName, content = Info}|T], Res) ->
    DataInfo = do_data_info(Info, [], #{}),
    data_info(T, [{?BINARY(SheetName), DataInfo}|Res]);
data_info([], Res) ->
    Res.

%% 数据页第一行是索引名称，第二行是索引描述，第三行开始是数据内容
do_data_info([#excel_cell{r = 1, c = C, v = V}|T], Keys, Values) ->
    do_data_info(T, [{C, V}|Keys], Values);
do_data_info([#excel_cell{r = 2}|T], Keys, Values) ->
    do_data_info(T, Keys, Values);
do_data_info([#excel_cell{r = R, c = C, v = V}|T], Keys, Values) ->
    L = maps:get(R, Values, []),
    do_data_info(T, Keys, Values#{R => [{C, V}|L]});
do_data_info([], Keys, Values) ->
    {KIndex, KMark} = lists:unzip(lists:keysort(1, Keys)),
    VL = lists:keysort(1, maps:to_list(Values)),
    lists:foldr(fun({_, RowList}, Acc) ->
        List = fill_by_index(KIndex, RowList, []),
        [maps:from_list(lists:zip(KMark, List))|Acc]
    end, [], VL).

fill_by_index([Index|T], List, Res) ->
    case lists:keytake(Index, 1, List) of
        {value, {_, Value}, NewList} ->
            fill_by_index(T, NewList, [Value|Res]);
        false ->
            fill_by_index(T, List, [undefined|Res])
    end;
fill_by_index([], _List, Res) ->
    lists:reverse(Res).
