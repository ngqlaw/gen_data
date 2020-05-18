%%%-------------------------------------------------------------------
%%% @author ngq <ngq_scut@126.com>
%%% @doc
%%% 数据生成
%%% @end
%%%-------------------------------------------------------------------
-module(gen_data).

-export([main/1]).

-include_lib("tools/include/excel.hrl").
-include_lib("tools/include/util.hrl").

-callback(init() -> {ok, State :: term()}).
-callback(translate(Sheets) -> DataInfo when
      Sheets :: [Sheet | OtherSheets],
      Sheet :: {SheetName :: binary(), RowInfo :: list()},
      OtherSheets :: term(),
      DataInfo :: map()).

main(_) ->
    io:format("===========Start gen data!===========~n"),
    [begin
        try
            io:format("try gen data by ~p......~n", [Module]),
            {ok, #{
                source := InputPath,
                template := TemplatePath,
                data := OutputPath
            }} = Module:init(),
            io:format("init ok!~n"),
            {ok, Sheets} = lib_excel:open(InputPath),
            SheetInfo = data_info(Sheets, []),
            io:format("parse ok!~n"),
            DataInfo = Module:translate(SheetInfo),
            Template = bbmustache:parse_file(TemplatePath),
            Content = bbmustache:compile(Template, DataInfo),
            file:write_file(OutputPath, Content),
            io:format("done!~n")
        catch
            E:R ->
                io:format("~p:~p~n", [E, R])
        end
    end || Module <- get_callback_mod()],
    io:format("===========Finish gen data!===========~n"),
    ok.


%% 数据转换执行模块
get_callback_mod() ->
    {ok, Path} = file:get_cwd(),
    L = find_by_dir([Path], []),
    AddPaths = find_ebin_by_dir([Path], []),
    code:add_paths(AddPaths),
    lists:filter(fun is_callback_mod/1, L).

is_callback_mod(Module) ->
    _Res = code:ensure_loaded(Module),
    Info = Module:module_info(attributes),
    lists:member({gen_data, [true]}, Info).

find_ebin_by_dir([Dir|T], Res) ->
    case file:list_dir(Dir) of
        {ok, L} ->
            NewRes = do_find_ebin_by_dir(L, Dir, Res),
            find_ebin_by_dir(T, NewRes);
        _E ->
            find_ebin_by_dir(T, Res)
    end;
find_ebin_by_dir([], Res) ->
    Res.

do_find_ebin_by_dir(["ebin"|T], Dir, Res) ->
    SubDir = filename:join([Dir, "ebin"]),
    case filelib:is_dir(SubDir) of
        true ->
            do_find_ebin_by_dir(T, Dir, [SubDir|Res]);
        false ->
            do_find_ebin_by_dir(T, Dir, Res)
    end;
do_find_ebin_by_dir([H|T], Dir, Res) ->
    SubDir = filename:join([Dir, H]),
    case filelib:is_dir(SubDir) of
        true ->
            NewRes = find_ebin_by_dir([SubDir], Res),
            do_find_ebin_by_dir(T, Dir, NewRes);
        false ->
            do_find_ebin_by_dir(T, Dir, Res)
    end;
do_find_ebin_by_dir([], _Dir, Res) ->
    Res.

find_by_dir([Dir|T], Res) ->
    case file:list_dir(Dir) of
        {ok, L} ->
            NewRes = do_find_by_dir(L, Dir, Res),
            find_by_dir(T, NewRes);
        _E ->
            find_by_dir(T, Res)
    end;
find_by_dir([], Res) ->
    Res.

do_find_by_dir([H|T], Dir, Res) ->
    case filename:extension(H) == ".beam" of
        true ->
            Module = ?ATOM(filename:basename(H, ".beam")),
            do_find_by_dir(T, Dir, [Module|Res]);
        false ->
            SubDir = filename:join([Dir, H]),
            case filelib:is_dir(SubDir) of
                true ->
                    NewRes = find_by_dir([SubDir], Res),
                    do_find_by_dir(T, Dir, NewRes);
                false ->
                    do_find_by_dir(T, Dir, Res)
            end
    end;
do_find_by_dir([], _Dir, Res) ->
    Res.

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
