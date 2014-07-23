%%% ====================================================================
%%% This software is copyright (c) 2006-2014, ProcessOne.
%%%
%%% @copyright 2006-2014 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

-module(cean_tools).
-author("Christophe Romain <christophe.romain@process-one.net>").

-export([update_pub/4]).

update_pub(Pub, Vsn, Date, Status) ->
    Entry = case Status of
        "good" -> {Vsn, Date, good};
        "test" -> {Vsn, Date, test};
        "bad" -> {Vsn, Date, bad};
        _ -> {Vsn, Date, undefined}
    end,
    case file:consult(Pub) of
        {ok, Spec} ->
            New = case lists:keysearch(versions, 1, Spec) of
                {value, {versions, Versions}} ->
                    lists:keyreplace(versions, 1, Spec, Versions++[Entry]);
                _ ->
                    Spec++[{versions, [Entry]}]
            end,
            IO = lists:foldl(
                fun(Rec, Acc) -> Acc++io_lib:format("~p.~n", [Rec]) end,
                [], New),
            ok == file:write_file(Pub, IO);
        _ ->
            false
    end.
