# gen_data

根据excel文件内容生成erl文件

# Build

    $ rebar3 escriptize

# 使用说明

## 使用mustache模板类型

## 构建自定义的解析生成模块，例如：

```
-module(Module).

-gen_data(true). % 用于标识属于数据生成模块

-export([init/0, translate/1]). % 回调函数

init() ->
    {ok, #{
        source => "path/to/source_file",
        template => "path/to/priv/template_file",
        data => "path/to/gen_file"
    }}.

%% 输入的是包含 {数据页名称(二进制), 数据列表(使用第一行数据作为map的关键字)} 的列表
translate(Sheets) ->
    do_translate(Sheets, #{}).

do_translate([{<<"test">>, Info}|T], Res) ->
    do_translate(T, Res#{"tt" => Info});
do_translate([_|T], Res) ->
    do_translate(T, Res);
do_translate([], Res) ->
    Res.
```

## 使用gen_data脚本生成预期文件
