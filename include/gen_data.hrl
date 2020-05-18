-ifndef(GEN_DATA_H).

-define(GEN_DATA_H, true).

-ifndef(INFO).
-define(INFO(Format), io:format(Format)).
-define(INFO(Format, Data), io:format(Format, Data)).
-endif.

-endif.