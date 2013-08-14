%% -*- coding: utf-8 -*-
%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2013, Vladimir G. Sekissov
%%% @doc
%%% Translation of Snowball russian stemmer to Erlang.
%%% @end
%%% Created : 13 Aug 2013 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(rustem).

-export([stem/1, translit/1, untranslit/1]).

-define(PERFECTIVE_GERUND_SUFFIXES, [<<"ivshis'">>, <<"yvshis'">>, <<"vshis'">>,
                                     <<"ivshi">>, <<"yvshi">>, <<"vshi">>, <<"iv">>,
                                     <<"yv">>, <<"v">>]).

-define(ADJECTIVAL_SUFFIXES, [<<"ui^ushchi^ui^u">>, <<"ui^ushchi^ai^a">>,
                              <<"ui^ushchimi">>, <<"ui^ushchymi">>, <<"ui^ushchego">>,
                              <<"ui^ushchogo">>, <<"ui^ushchemu">>, <<"ui^ushchomu">>,
                              <<"ui^ushchikh">>, <<"ui^ushchykh">>,
                              <<"ui^ushchui^u">>, <<"ui^ushchaia">>,
                              <<"ui^ushchoi^u">>, <<"ui^ushchei^u">>,
                              <<"i^ushchi^ui^u">>, <<"i^ushchi^ai^a">>,
                              <<"ui^ushchee">>, <<"ui^ushchie">>,
                              <<"ui^ushchye">>, <<"ui^ushchoe">>, <<"ui^ushchei`">>,
                              <<"ui^ushchii`">>, <<"ui^ushchyi`">>,
                              <<"ui^ushchoi`">>, <<"ui^ushchem">>, <<"ui^ushchim">>,
                              <<"ui^ushchym">>, <<"ui^ushchom">>, <<"i^ushchimi">>,
                              <<"i^ushchymi">>, <<"i^ushchego">>, <<"i^ushchogo">>,
                              <<"i^ushchemu">>, <<"i^ushchomu">>, <<"i^ushchikh">>,
                              <<"i^ushchykh">>, <<"i^ushchui^u">>, <<"i^ushchai^a">>,
                              <<"i^ushchoi^u">>, <<"i^ushchei^u">>, <<"i^ushchee">>,
                              <<"i^ushchie">>, <<"i^ushchye">>, <<"i^ushchoe">>,
                              <<"i^ushchei`">>, <<"i^ushchii`">>,
                              <<"i^ushchyi`">>, <<"i^ushchoi`">>, <<"i^ushchem">>,
                              <<"i^ushchim">>, <<"i^ushchym">>, <<"i^ushchom">>,
                              <<"shchi^ui^u">>, <<"shchi^ai^a">>, <<"ivshi^ui^u">>,
                              <<"ivshi^ai^a">>, <<"yvshi^ui^u">>, <<"yvshi^ai^a">>,
                              <<"shchimi">>, <<"shchymi">>, <<"shchego">>, <<"shchogo">>,
                              <<"shchemu">>, <<"shchomu">>, <<"shchikh">>, <<"shchykh">>,
                              <<"shchui^u">>, <<"shchai^a">>, <<"shchoi^u">>,
                              <<"shchei^u">>, <<"ivshimi">>, <<"ivshymi">>,
                              <<"ivshego">>, <<"ivshogo">>, <<"ivshemu">>, <<"ivshomu">>,
                              <<"ivshikh">>, <<"ivshykh">>, <<"ivshui^u">>,
                              <<"ivshai^a">>, <<"ivshoi^u">>, <<"ivshei^u">>,
                              <<"yvshimi">>, <<"yvshymi">>, <<"yvshego">>, <<"yvshogo">>,
                              <<"yvshemu">>, <<"yvshomu">>, <<"yvshikh">>, <<"yvshykh">>,
                              <<"yvshui^u">>, <<"yvshai^a">>, <<"yvshoi^u">>,
                              <<"yvshei^u">>, <<"vshi^ui^u">>, <<"vshi^ai^a">>,
                              <<"shchee">>, <<"shchie">>, <<"shchye">>, <<"shchoe">>,
                              <<"shchei`">>, <<"shchii`">>, <<"shchyi`">>, <<"shchoi`">>,
                              <<"shchem">>, <<"shchim">>, <<"shchym">>, <<"shchom">>,
                              <<"ivshee">>, <<"ivshie">>, <<"ivshye">>, <<"ivshoe">>,
                              <<"ivshei`">>, <<"ivshii`">>, <<"ivshyi`">>,
                              <<"ivshoi`">>, <<"ivshem">>, <<"ivshim">>, <<"ivshym">>,
                              <<"ivshom">>, <<"yvshee">>, <<"yvshie">>, <<"yvshye">>,
                              <<"yvshoe">>, <<"yvshei`">>, <<"yvshii`">>,
                              <<"yvshyi`">>, <<"yvshoi`">>, <<"yvshem">>,
                              <<"yvshim">>, <<"yvshym">>, <<"yvshom">>, <<"vshimi">>,
                              <<"vshymi">>, <<"vshego">>, <<"vshogo">>, <<"vshemu">>,
                              <<"vshomu">>, <<"vshikh">>, <<"vshykh">>, <<"vshui^u">>,
                              <<"vshai^a">>, <<"vshoi^u">>, <<"vshei^u">>,
                              <<"emi^ui^u">>, <<"emi^ai^a">>, <<"nni^ui^u">>,
                              <<"nni^ai^a">>, <<"vshee">>,
                              <<"vshie">>, <<"vshye">>, <<"vshoe">>, <<"vshei`">>,
                              <<"vshii`">>, <<"vshyi`">>, <<"vshoi`">>,
                              <<"vshem">>, <<"vshim">>, <<"vshym">>, <<"vshom">>,
                              <<"emimi">>, <<"emymi">>, <<"emego">>, <<"emogo">>,
                              <<"ememu">>, <<"emomu">>, <<"emikh">>, <<"emykh">>,
                              <<"emui^u">>, <<"emai^a">>, <<"emoi^u">>, <<"emei^u">>,
                              <<"nnimi">>, <<"nnymi">>, <<"nnego">>, <<"nnogo">>,
                              <<"nnemu">>, <<"nnomu">>, <<"nnikh">>, <<"nnykh">>,
                              <<"nnui^u">>, <<"nnai^a">>, <<"nnoi^u">>, <<"nnei^u">>,
                              <<"emee">>, <<"emie">>, <<"emye">>, <<"emoe">>,
                              <<"emei`">>, <<"emii`">>, <<"emyi`">>,
                              <<"emoi`">>, <<"emem">>, <<"emim">>, <<"emym">>,
                              <<"emom">>, <<"nnee">>, <<"nnie">>, <<"nnye">>, <<"nnoe">>,
                              <<"nnei`">>, <<"nnii`">>, <<"nnyi`">>,
                              <<"nnoi`">>, <<"nnem">>, <<"nnim">>, <<"nnym">>,
                              <<"nnom">>, <<"i^ui^u">>, <<"i^ai^a">>, <<"imi">>, <<"ymi">>,
                              <<"ego">>, <<"ogo">>, <<"emu">>, <<"omu">>, <<"ikh">>,
                              <<"ykh">>, <<"ui^u">>, <<"ai^a">>, <<"oi^u">>, <<"ei^u">>,
                              <<"ee">>, <<"ie">>, <<"ye">>, <<"oe">>, <<"ei`">>,
                              <<"ii`">>, <<"yi`">>, <<"oi`">>, <<"em">>,
                              <<"im">>, <<"ym">>, <<"om">>]).

-define(REFLEXIVE_SUFFIXES, [<<"si^a">>, <<"s'">>]).

-define(VERB_SUFFIXES, [<<"esh'">>, <<"ei`te">>, <<"ui`te">>, <<"ui^ut">>,
                        <<"ish'">>, <<"ete">>, <<"i`te">>, <<"i^ut">>, <<"nno">>,
                        <<"ila">>, <<"yla">>, <<"ena">>, <<"ite">>, <<"ili">>, <<"yli">>,
                        <<"ilo">>, <<"ylo">>, <<"eno">>, <<"i^at">>, <<"uet">>, <<"eny">>,
                        <<"it'">>, <<"yt'">>, <<"ui^u">>, <<"la">>, <<"na">>, <<"li">>,
                        <<"em">>, <<"lo">>, <<"no">>, <<"et">>, <<"ny">>, <<"t'">>,
                        <<"ei`">>, <<"ui`">>, <<"il">>, <<"yl">>, <<"im">>,
                        <<"ym">>, <<"en">>, <<"it">>, <<"yt">>, <<"i^u">>, <<"i`">>,
                        <<"l">>, <<"n">>]).

-define(NOUN_SUFFIXES, [<<"ii^ami">>, <<"ii^akh">>, <<"i^ami">>, <<"ii^am">>, <<"i^akh">>,
                        <<"ami">>, <<"iei`">>, <<"i^am">>, <<"iem">>, <<"akh">>,
                        <<"ii^u">>, <<"'i^u">>, <<"ii^a">>, <<"'i^a">>, <<"ev">>, <<"ov">>,
                        <<"ie">>, <<"'e">>, <<"ei">>, <<"ii">>, <<"ei`">>,
                        <<"oi`">>, <<"ii`">>, <<"em">>, <<"am">>, <<"om">>,
                        <<"i^u">>, <<"i^a">>, <<"a">>, <<"e">>, <<"i">>, <<"i`">>,
                        <<"o">>, <<"u">>, <<"y">>, <<"'">>]).

-define(SUPERLATIVE_SUFFIXES, [<<"ei`she">>, <<"ei`sh">>]).

-define(DERIVATIONAL_SUFFIXES, [<<"ost'">>, <<"ost">>]).

-define(SUFFIXES1, [<<"i^ushchi^ui^u">>, <<"i^ushchi^ai^a">>,
                    <<"i^ushchui^u">>, <<"i^ushchai^a">>, <<"i^ushchoi^u">>,
                    <<"i^ushchei^u">>, <<"i^ushchimi">>, <<"i^ushchymi">>,
                    <<"i^ushchego">>, <<"i^ushchogo">>, <<"i^ushchemu">>,
                    <<"i^ushchomu">>, <<"i^ushchikh">>, <<"i^ushchykh">>,
                    <<"shchi^ui^u">>, <<"shchi^ai^a">>, <<"i^ushchee">>,
                    <<"i^ushchie">>, <<"i^ushchye">>, <<"i^ushchoe">>,
                    <<"i^ushchei`">>, <<"i^ushchii`">>, <<"i^ushchyi`">>,
                    <<"i^ushchoi`">>, <<"i^ushchem">>, <<"i^ushchim">>,
                    <<"i^ushchym">>, <<"i^ushchom">>, <<"vshi^ui^u">>,
                    <<"vshi^ai^a">>, <<"shchui^u">>, <<"shchai^a">>,
                    <<"shchoi^u">>, <<"shchei^u">>, <<"emi^ui^u">>,
                    <<"emi^ai^a">>, <<"nni^ui^u">>, <<"nni^ai^a">>,
                    <<"shchimi">>, <<"shchymi">>, <<"shchego">>, <<"shchogo">>,
                    <<"shchemu">>, <<"shchomu">>, <<"shchikh">>, <<"shchykh">>,
                    <<"vshui^u">>, <<"vshai^a">>, <<"vshoi^u">>, <<"vshei^u">>,
                    <<"shchee">>, <<"shchie">>, <<"shchye">>, <<"shchoe">>,
                    <<"shchei`">>, <<"shchii`">>, <<"shchyi`">>, <<"shchoi`">>,
                    <<"shchem">>, <<"shchim">>, <<"shchym">>, <<"shchom">>,
                    <<"vshimi">>, <<"vshymi">>, <<"vshego">>, <<"vshogo">>,
                    <<"vshemu">>, <<"vshomu">>, <<"vshikh">>, <<"vshykh">>,
                    <<"emui^u">>, <<"emai^a">>, <<"emoi^u">>, <<"emei^u">>,
                    <<"nnui^u">>, <<"nnai^a">>, <<"nnoi^u">>, <<"nnei^u">>,
                    <<"vshee">>, <<"vshie">>, <<"vshye">>, <<"vshoe">>,
                    <<"vshei`">>, <<"vshii`">>, <<"vshyi`">>, <<"vshoi`">>,
                    <<"vshem">>, <<"vshim">>, <<"vshym">>, <<"vshom">>,
                    <<"emimi">>, <<"emymi">>, <<"emego">>, <<"emogo">>,
                    <<"ememu">>, <<"emomu">>, <<"emikh">>, <<"emykh">>,
                    <<"nnimi">>, <<"nnymi">>, <<"nnego">>, <<"nnogo">>,
                    <<"nnemu">>, <<"nnomu">>, <<"nnikh">>, <<"nnykh">>,
                    <<"emee">>, <<"emie">>, <<"emye">>, <<"emoe">>, <<"emei`">>,
                    <<"emii`">>, <<"emyi`">>, <<"emoi`">>, <<"emem">>, <<"emim">>,
                    <<"emym">>, <<"emom">>, <<"nnee">>, <<"nnie">>, <<"nnye">>,
                    <<"nnoe">>, <<"nnei`">>, <<"nnii`">>, <<"nnyi`">>, <<"nnoi`">>,
                    <<"nnem">>, <<"nnim">>, <<"nnym">>, <<"nnom">>]).

-define(SUFFIXES2, [<<"la">>, <<"na">>, <<"ete">>, <<"i`te">>, <<"li">>,
                    <<"i`">>, <<"l">>, <<"em">>, <<"n">>, <<"lo">>, <<"no">>,
                    <<"et">>, <<"i^ut">>, <<"ny">>, <<"t'">>, <<"esh'">>,
                    <<"nno">>]).

-define(VOWVELS, [<<"i^a">>, <<"i^u">>, <<"e`">>,
                  <<"a">>, <<"e">>, <<"i">>, <<"o">>, <<"u">>, <<"y">>]).

%%@doc
%% Stem a Russian word and return the stemmed form.
%%@end
stem(Word) when is_binary(Word) ->
    Word1 = translit(Word),
    {RV, R2} = regions(Word1),
    untranslit(stem1({Word1, RV, R2})).

stem1(Arg) ->
    {Word1, RV1, R2_1} =
        apply_while(Arg, [fun stem_strip_gerund/1,
                          fun (A) ->
                                  apply_while(
                                    apply_default(A, fun stem_strip_reflexive/1, Arg),
                                    [fun stem_strip_adjectival/1,
                                     fun stem_strip_verb/1,
                                     fun stem_strip_noun/1
                                    ])
                          end
                         ]),
    [Word2, R2_2] = strip_suffixes(RV1, [Word1, R2_1], [<<"i">>]),
    [Word3] = strip_suffixes(R2_2, [Word2], ?DERIVATIONAL_SUFFIXES),
    Word4 = case is_suffix(<<"nn">>, Word3) of
                true ->
                    binary:part(Word3, 0, byte_size(Word3) - 1);
                false ->
                    stem_strip_superlative(Word3)
            end,
    Word4.

stem_strip_gerund(Arg) ->
    StripF =
        fun (A, Suf) ->
                case lists:member(Suf, [<<"v">>, <<"vshi">>, <<"vshis'">>]) of
                    true ->
                        strip_on_ya(A, Suf);
                    false ->
                        stem_strip_suffix(A, byte_size(Suf))
                end
        end,
    stem_fold_suffixes(StripF, Arg, ?PERFECTIVE_GERUND_SUFFIXES).

stem_strip_reflexive(Arg) ->
    StripF = fun (A, Suf) ->
                     stem_strip_suffix(A, byte_size(Suf))
             end,
    stem_fold_suffixes(StripF, Arg, ?REFLEXIVE_SUFFIXES).

stem_strip_adjectival(Arg) ->
    StripF = fun (A, Suf) ->
                     SufLen = byte_size(Suf),
                     case lists:member(Suf, ?SUFFIXES1) of
                         false ->
                             stem_strip_suffix(A, SufLen);
                         true ->
                             strip_on_ya(A, Suf)
                     end
             end,
    stem_fold_suffixes(StripF, Arg, ?ADJECTIVAL_SUFFIXES).

stem_strip_verb(Arg) ->
    StripF = fun (A, Suf) ->
                     SufLen = byte_size(Suf),
                     case lists:member(Suf, ?SUFFIXES2) of
                         false ->
                             stem_strip_suffix(A, SufLen);
                         true ->
                             strip_on_ya(A, Suf)
                     end
             end,
    stem_fold_suffixes(StripF, Arg, ?VERB_SUFFIXES).

stem_strip_noun(Arg) ->
    StripF = fun (A, Suf) ->
                     stem_strip_suffix(A, byte_size(Suf))
             end,
    stem_fold_suffixes(StripF, Arg, ?NOUN_SUFFIXES).

stem_strip_superlative(Word) ->
    case lists:dropwhile(fun (S) -> not is_suffix(S, Word) end,
                         ?SUPERLATIVE_SUFFIXES) of
        [] ->
            [Ret] = strip_suffixes(Word, [Word], [<<"'">>]),
            Ret;
        [Suf|_] ->
            binary:part(Word, 0, byte_size(Word) - byte_size(Suf))
    end.

strip_on_ya(Arg={_, RV, _}, Suffix) ->
    SufLen = byte_size(Suffix),
    RVLen = byte_size(RV),
    SufPos = RVLen - SufLen,
    Len1 = if SufPos >= 1 -> 1; true -> 0 end,
    Len2 = if SufPos >= 3 -> 3; true -> 0 end,
    case (binary:part(RV, SufPos, -Len2) == <<"i^a">> orelse
          binary:part(RV, SufPos, -Len1) == <<"a">>) of
        true ->
            stem_strip_suffix(Arg, SufLen);
        false ->
            false
    end.

stem_strip_suffix({Word, RV, R2}, SufLen) ->
    {binary_part_weak(Word, 0, byte_size(Word) - SufLen),
     binary_part_weak(RV, 0, byte_size(RV) - SufLen),
     binary_part_weak(R2, 0, byte_size(R2) - SufLen)
    }.

binary_part_weak(Bin, Pos, Len) ->
    if (Pos + Len) < 0 ->
            <<>>;
       (Pos + Len) > byte_size(Bin) ->
            binary:part(Bin, Pos, byte_size(Bin) - Pos);
       true ->
            binary:part(Bin, Pos, Len)
    end.

strip_suffixes(Word, Words, Suffixes) ->
    case lists:dropwhile(fun (S) -> not is_suffix(S, Word) end, Suffixes) of
        [] ->
            Words;
        [Suf|_] ->
            [binary:part(W, 0, byte_size(W) - byte_size(Suf)) || W <- Words]
    end.

is_suffix(Suf, Str) ->
    binary:longest_common_suffix([Str, Suf]) == byte_size(Suf).

stem_fold_suffixes(Fun, Acc={_, RV, _}, Suffixes) ->
    case lists:dropwhile(fun (S) -> not is_suffix(S, RV) end,
                         Suffixes) of
        [] ->
            false;
        [Suf|Rest] ->
            case Fun(Acc, Suf) of
                false ->
                    stem_fold_suffixes(Fun, Acc, Rest);
                Res ->
                    Res
            end
    end.

apply_default(Arg, Fn, Default) ->
    case Fn(Arg) of
        false ->
            Default;
        Res ->
            Res
    end.

apply_while(Arg, []) ->
    Arg;
apply_while(Arg, [Fn|Rest]) ->
    case Fn(Arg) of
        false ->
            apply_while(Arg, Rest);
        Res ->
            Res
    end.

%%@doc
%%  Return the regions RV and R2 which are used by the Russian stemmer.
%%
%% In any word, RV is the region after the first vowel,
%% or the end of the word if it contains no vowel.
%%
%% R2 is the region after the first non-vowel following
%% a vowel in R1, or the end of the word if there is no such non-vowel.
%%
%% R1 is the region after the first non-vowel following a vowel,
%% or the end of the word if there is no such non-vowel.
%%@end
regions(Word) ->
    R1 = regions_r1(Word),
    R2 = regions_r1(R1),
    {_, RV} = split_on_vowvel(Word),
    {RV, R2}.

regions_r1(Word) ->
    {S1, S2} = split_on_vowvel(Word),
    case S2 of
        <<>> ->
            S1;
        _ ->
            case start_with_vowvel(S2) of
                false ->
                    S2;
                _ ->
                    regions_r1(S2)
            end
    end.

start_with_vowvel(Str) ->
    case lists:dropwhile(fun (V) ->
                                 not (binary:longest_common_prefix([V, Str]) == size(V))
                         end, ?VOWVELS) of
        [] ->
            false;
        [V|_] ->
            size(V)
    end.

split_on_vowvel(Str) ->
    split_on_vowvel(Str, <<>>).

split_on_vowvel(<<>>, Acc) ->
    {Acc, <<>>};
split_on_vowvel(Str, Acc) ->
    case start_with_vowvel(Str) of
        false ->
            << C, Rest/binary >> = Str,
            split_on_vowvel(Rest, << Acc/binary, C >>);
        Size ->
            << Chr:Size/binary, Rest/binary >> = Str,
            {<< Acc/binary, Chr/binary >>, Rest}
    end.

%%@doc
%% Transliterate a Russian word into the Latin alphabet.
%%@end
translit(Word) ->
    translit(Word, <<>>).

translit(<<>>, Acc) ->
    Acc;
translit(<<C/utf8, Rest/binary>>, Acc) ->
    translit(Rest, << Acc/binary, (translit_chr(C))/binary>>).

untranslit(Word) ->
    untranslit(Word, <<>>).

untranslit(<<>>, Acc) ->
    Acc;
untranslit(Str, Acc) ->
    {ChrBin, Rest} = untranslit_lookup(4, Str),
    untranslit(Rest, << Acc/binary, ChrBin/utf8>>).

untranslit_lookup(0, <<C/utf8, Rest/binary>>) ->
    {C, Rest};
untranslit_lookup(Len, Str) ->
    case Str of
        << From:Len/binary, Rest/binary >> ->
            case untranslit_chr(From) of
                false ->
                    untranslit_lookup(Len - 1, Str);
                To ->
                    {To, Rest}
            end;
        _ ->
            untranslit_lookup(Len - 1, Str)
    end.

untranslit_chr(<<"a">>) -> 16#0430;
untranslit_chr(<<"b">>) -> 16#0431;
untranslit_chr(<<"v">>) -> 16#0432;
untranslit_chr(<<"g">>) -> 16#0433;
untranslit_chr(<<"d">>) -> 16#0434;
untranslit_chr(<<"e">>) -> 16#0435;
untranslit_chr(<<"zh">>) -> 16#0436;
untranslit_chr(<<"z">>) -> 16#0437;
untranslit_chr(<<"i">>) -> 16#0438;
untranslit_chr(<<"i`">>) -> 16#0439;
untranslit_chr(<<"k">>) -> 16#043A;
untranslit_chr(<<"l">>) -> 16#043B;
untranslit_chr(<<"m">>) -> 16#043C;
untranslit_chr(<<"n">>) -> 16#043D;
untranslit_chr(<<"o">>) -> 16#043E;
untranslit_chr(<<"p">>) -> 16#043F;
untranslit_chr(<<"r">>) -> 16#0440;
untranslit_chr(<<"s">>) -> 16#0441;
untranslit_chr(<<"t">>) -> 16#0442;
untranslit_chr(<<"u">>) -> 16#0443;
untranslit_chr(<<"f">>) -> 16#0444;
untranslit_chr(<<"kh">>) -> 16#0445;
untranslit_chr(<<"t^s">>) -> 16#0446;
untranslit_chr(<<"ch">>) -> 16#0447;
untranslit_chr(<<"sh">>) -> 16#0448;
untranslit_chr(<<"shch">>) -> 16#0449;
untranslit_chr(<<"''">>) -> 16#044A;
untranslit_chr(<<"y">>) -> 16#044B;
untranslit_chr(<<"'">>) -> 16#044C;
untranslit_chr(<<"e`">>) -> 16#044D;
untranslit_chr(<<"i^u">>) -> 16#044E;
untranslit_chr(<<"i^a">>) -> 16#044F;
untranslit_chr(_) ->
    false.

translit_chr(16#0410) -> <<"a">>;
translit_chr(16#0430) -> <<"a">>;
translit_chr(16#0411) -> <<"b">>;
translit_chr(16#0431) -> <<"b">>;
translit_chr(16#0412) -> <<"v">>;
translit_chr(16#0432) -> <<"v">>;
translit_chr(16#0413) -> <<"g">>;
translit_chr(16#0433) -> <<"g">>;
translit_chr(16#0414) -> <<"d">>;
translit_chr(16#0434) -> <<"d">>;
translit_chr(16#0415) -> <<"e">>;
translit_chr(16#0435) -> <<"e">>;
translit_chr(16#0401) -> <<"e">>;
translit_chr(16#0451) -> <<"e">>;
translit_chr(16#0416) -> <<"zh">>;
translit_chr(16#0436) -> <<"zh">>;
translit_chr(16#0417) -> <<"z">>;
translit_chr(16#0437) -> <<"z">>;
translit_chr(16#0418) -> <<"i">>;
translit_chr(16#0438) -> <<"i">>;
translit_chr(16#0419) -> <<"i`">>;
translit_chr(16#0439) -> <<"i`">>;
translit_chr(16#041A) -> <<"k">>;
translit_chr(16#043A) -> <<"k">>;
translit_chr(16#041B) -> <<"l">>;
translit_chr(16#043B) -> <<"l">>;
translit_chr(16#041C) -> <<"m">>;
translit_chr(16#043C) -> <<"m">>;
translit_chr(16#041D) -> <<"n">>;
translit_chr(16#043D) -> <<"n">>;
translit_chr(16#041E) -> <<"o">>;
translit_chr(16#043E) -> <<"o">>;
translit_chr(16#041F) -> <<"p">>;
translit_chr(16#043F) -> <<"p">>;
translit_chr(16#0420) -> <<"r">>;
translit_chr(16#0440) -> <<"r">>;
translit_chr(16#0421) -> <<"s">>;
translit_chr(16#0441) -> <<"s">>;
translit_chr(16#0422) -> <<"t">>;
translit_chr(16#0442) -> <<"t">>;
translit_chr(16#0423) -> <<"u">>;
translit_chr(16#0443) -> <<"u">>;
translit_chr(16#0424) -> <<"f">>;
translit_chr(16#0444) -> <<"f">>;
translit_chr(16#0425) -> <<"kh">>;
translit_chr(16#0445) -> <<"kh">>;
translit_chr(16#0426) -> <<"t^s">>;
translit_chr(16#0446) -> <<"t^s">>;
translit_chr(16#0427) -> <<"ch">>;
translit_chr(16#0447) -> <<"ch">>;
translit_chr(16#0428) -> <<"sh">>;
translit_chr(16#0448) -> <<"sh">>;
translit_chr(16#0429) -> <<"shch">>;
translit_chr(16#0449) -> <<"shch">>;
translit_chr(16#042A) -> <<"''">>;
translit_chr(16#044A) -> <<"''">>;
translit_chr(16#042B) -> <<"y">>;
translit_chr(16#044B) -> <<"y">>;
translit_chr(16#042C) -> <<"'">>;
translit_chr(16#044C) -> <<"'">>;
translit_chr(16#042D) -> <<"e`">>;
translit_chr(16#044D) -> <<"e`">>;
translit_chr(16#042E) -> <<"i^u">>;
translit_chr(16#044E) -> <<"i^u">>;
translit_chr(16#042F) -> <<"i^a">>;
translit_chr(16#044F) -> <<"i^a">>;
translit_chr(C) -> <<C/utf8>>.
