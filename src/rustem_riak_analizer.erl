%%% -*- coding: utf-8 -*-
%%% @author Vladimir G. Sekissov <eryx67@gmail.com>
%%% @copyright (C) 2013, Vladimir G. Sekissov
%%% @doc
%%% Riak analizer
%%% @end
%%% Created :  2 Oct 2013 by Vladimir G. Sekissov <eryx67@gmail.com>

-module(rustem_riak_analizer).


-export([standard_analyzer_factory/2]).

-define(UPPERCHAR(C), (C >= $A andalso C =< $Z) orelse (C >= 16#0410 andalso C =< 16#042F)).
-define(LOWERCHAR(C), (C >= $a andalso C =< $z) orelse (C >= 16#0430 andalso C =< 16#044F)).
-define(NUMBER(C), (C >= $0 andalso C =< $9)).
-define(WHITESPACE(C), ((C == $\s) orelse (C == $\n) orelse (C == $\t) orelse (C == $\f) orelse (C == $\r) orelse (C == $\v))).

standard_analyzer_factory(Text, [MinLengthArg]) ->
    MinLength = list_to_integer(MinLengthArg),
    {ok, standard(Text, MinLength, <<>>, [], 0)};
standard_analyzer_factory(Text, _Other) ->
    {ok, lists:reverse(standard(Text, 3, <<>>, [], 0))}.

standard(<<H/utf8, T/binary>>, MinLength, Acc, ResultAcc, AccLen) when ?UPPERCHAR(H) ->
    H1 = H + 32,
    standard(T, MinLength, <<Acc/binary, H1/utf8 >>, ResultAcc, AccLen + 1);
standard(<<H/utf8, T/binary>>, MinLength, Acc, ResultAcc, AccLen) when ?LOWERCHAR(H) orelse ?NUMBER(H) ->
    standard(T, MinLength, <<Acc/binary, H/utf8 >>, ResultAcc, AccLen + 1);
standard(<<$., H/utf8, T/binary>>, MinLength, Acc, ResultAcc, AccLen) when ?UPPERCHAR(H) ->
    H1 = H + 32,
    standard(T, MinLength, <<Acc/binary, $., H1/utf8>>, ResultAcc, AccLen + 2);
standard(<<$., H/utf8, T/binary>>, MinLength, Acc, ResultAcc, AccLen) when ?LOWERCHAR(H) orelse ?NUMBER(H) ->
    standard(T, MinLength, <<Acc/binary, $., H/utf8>>, ResultAcc, AccLen + 2);
standard(<<$., H/utf8, T/binary>>, MinLength, Acc, ResultAcc, AccLen) ->
    standard(T, MinLength, <<Acc/binary, $., H/utf8>>, ResultAcc, AccLen + 2);
standard(<<_X/utf8, T/binary>>, MinLength, Acc, ResultAcc, AccLen) ->
    standard_termify(T, MinLength, Acc, ResultAcc, AccLen);
standard(<<>>, MinLength, Acc, ResultAcc, AccLen) ->
    standard_termify(<<>>, MinLength, Acc, ResultAcc, AccLen).

%% Determine if this term is valid, if so, add it to the list we are
%% generating.
standard_termify(<<>>, _MinLength, <<>>, ResultAcc, _AL) ->
    ResultAcc;
standard_termify(T, MinLength, <<>>, ResultAcc, _AL) ->
    standard(T, MinLength, <<>>, ResultAcc, 0);
standard_termify(T, MinLength, _A, ResultAcc, AccLen) when AccLen < MinLength ->
    standard(T, MinLength, <<>>, ResultAcc, 0);
standard_termify(T, MinLength, Term, ResultAcc, TermLen) ->
    NewResultAcc = case is_stopword(Term, TermLen) of
                       false ->
                           RuStem = rustem:stem(Term),
                           [RuStem | ResultAcc];
                       true ->
                           [skip|ResultAcc]
                   end,
    standard(T, MinLength, <<>>, NewResultAcc, 0).

is_stopword(Term, 1) ->
  ordsets:is_element(Term,
                     [<<"а"/utf8>>,<<"в"/utf8>>,<<"ж"/utf8>>,<<"и"/utf8>>,
                      <<"к"/utf8>>,<<"о"/utf8>>,<<"с"/utf8>>,<<"у"/utf8>>,
                      <<"я"/utf8>>]);
is_stopword(Term, 2) ->
  ordsets:is_element(Term,
                     [<<"бы"/utf8>>,<<"во"/utf8>>,<<"вы"/utf8>>,<<"да"/utf8>>,<<"до"/utf8>>,<<"ее"/utf8>>,<<"ей"/utf8>>,<<"ею"/utf8>>,<<"её"/utf8>>,<<"же"/utf8>>,<<"за"/utf8>>,<<"из"/utf8>>,<<"им"/utf8>>,<<"их"/utf8>>,<<"ли"/utf8>>,<<"мы"/utf8>>,<<"на"/utf8>>,<<"не"/utf8>>,<<"ни"/utf8>>,<<"но"/utf8>>,<<"ну"/utf8>>,<<"об"/utf8>>,<<"он"/utf8>>,<<"от"/utf8>>,<<"по"/utf8>>,<<"со"/utf8>>,<<"та"/utf8>>,<<"те"/utf8>>,<<"то"/utf8>>,<<"ту"/utf8>>,<<"ты"/utf8>>,<<"уж"/utf8>>,<<"эи"/utf8>>]);
is_stopword(Term, 3) ->
  ordsets:is_element(Term,
                     [<<"без"/utf8>>,<<"буд"/utf8>>,<<"быв"/utf8>>,<<"был"/utf8>>,<<"вам"/utf8>>,<<"вас"/utf8>>,<<"вот"/utf8>>,<<"все"/utf8>>,<<"всю"/utf8>>,<<"вся"/utf8>>,<<"всё"/utf8>>,<<"где"/utf8>>,<<"два"/utf8>>,<<"дел"/utf8>>,<<"для"/utf8>>,<<"его"/utf8>>,<<"ему"/utf8>>,<<"еще"/utf8>>,<<"ещё"/utf8>>,<<"или"/utf8>>,<<"име"/utf8>>,<<"ими"/utf8>>,<<"как"/utf8>>,<<"кто"/utf8>>,<<"мне"/utf8>>,<<"мог"/utf8>>,<<"мож"/utf8>>,<<"мой"/utf8>>,<<"моя"/utf8>>,<<"над"/utf8>>,<<"нам"/utf8>>,<<"нас"/utf8>>,<<"нее"/utf8>>,<<"ней"/utf8>>,<<"нет"/utf8>>,<<"нею"/utf8>>,<<"неё"/utf8>>,<<"ним"/utf8>>,<<"них"/utf8>>,<<"нэи"/utf8>>,<<"она"/utf8>>,<<"они"/utf8>>,<<"оно"/utf8>>,<<"под"/utf8>>,<<"при"/utf8>>,<<"про"/utf8>>,<<"раз"/utf8>>,<<"сам"/utf8>>,<<"так"/utf8>>,<<"там"/utf8>>,<<"тем"/utf8>>,<<"тех"/utf8>>,<<"той"/utf8>>,<<"том"/utf8>>,<<"тот"/utf8>>,<<"тою"/utf8>>,<<"три"/utf8>>,<<"тут"/utf8>>,<<"уже"/utf8>>,<<"уме"/utf8>>,<<"хот"/utf8>>,<<"хоч"/utf8>>,<<"чем"/utf8>>,<<"что"/utf8>>,<<"чём"/utf8>>,<<"эта"/utf8>>,<<"эти"/utf8>>,<<"это"/utf8>>,<<"эту"/utf8>>,<<"эты"/utf8>>]);
is_stopword(Term, 4) ->
  ordsets:is_element(Term,
                     [<<"была"/utf8>>,<<"были"/utf8>>,<<"было"/utf8>>,<<"быть"/utf8>>,<<"вами"/utf8>>,<<"ведь"/utf8>>,<<"весь"/utf8>>,<<"всей"/utf8>>,<<"всем"/utf8>>,<<"всех"/utf8>>,<<"всею"/utf8>>,<<"даже"/utf8>>,<<"долж"/utf8>>,<<"если"/utf8>>,<<"есть"/utf8>>,<<"куда"/utf8>>,<<"меня"/utf8>>,<<"мной"/utf8>>,<<"мною"/utf8>>,<<"можн"/utf8>>,<<"мочь"/utf8>>,<<"надо"/utf8>>,<<"нами"/utf8>>,<<"него"/utf8>>,<<"нему"/utf8>>,<<"ними"/utf8>>,<<"нужн"/utf8>>,<<"один"/utf8>>,<<"сама"/utf8>>,<<"сами"/utf8>>,<<"само"/utf8>>,<<"саму"/utf8>>,<<"свою"/utf8>>,<<"себе"/utf8>>,<<"себя"/utf8>>,<<"суть"/utf8>>,<<"тебе"/utf8>>,<<"тебя"/utf8>>,<<"теми"/utf8>>,<<"того"/utf8>>,<<"тоже"/utf8>>,<<"тому"/utf8>>,<<"хоть"/utf8>>,<<"чего"/utf8>>,<<"чтоб"/utf8>>,<<"чуть"/utf8>>,<<"этим"/utf8>>,<<"этих"/utf8>>,<<"этой"/utf8>>,<<"этом"/utf8>>,<<"этот"/utf8>>,<<"этою"/utf8>>]);
is_stopword(Term, 5) ->
  ordsets:is_element(Term,
                     [<<"более"/utf8>>,<<"будет"/utf8>>,<<"будто"/utf8>>,<<"вдруг"/utf8>>,<<"всего"/utf8>>,<<"всеми"/utf8>>,<<"всему"/utf8>>,<<"жизнь"/utf8>>,<<"зачем"/utf8>>,<<"здесь"/utf8>>,<<"какая"/utf8>>,<<"какой"/utf8>>,<<"когда"/utf8>>,<<"лучше"/utf8>>,<<"между"/utf8>>,<<"много"/utf8>>,<<"может"/utf8>>,<<"можно"/utf8>>,<<"опять"/utf8>>,<<"перед"/utf8>>,<<"после"/utf8>>,<<"потом"/utf8>>,<<"почти"/utf8>>,<<"разве"/utf8>>,<<"самим"/utf8>>,<<"самих"/utf8>>,<<"самой"/utf8>>,<<"самом"/utf8>>,<<"самою"/utf8>>,<<"собой"/utf8>>,<<"собою"/utf8>>,<<"такой"/utf8>>,<<"тобой"/utf8>>,<<"тобою"/utf8>>,<<"тогда"/utf8>>,<<"через"/utf8>>,<<"чтобы"/utf8>>,<<"этими"/utf8>>,<<"этого"/utf8>>,<<"этому"/utf8>>]);
is_stopword(Term, 6) ->
    ordsets:is_element(Term,
                       [<<"больше"/utf8>>,<<"всегда"/utf8>>,<<"другой"/utf8>>,<<"иногда"/utf8>>,<<"нельзя"/utf8>>,<<"нибудь"/utf8>>,<<"ничего"/utf8>>,<<"однако"/utf8>>,<<"потому"/utf8>>,<<"самими"/utf8>>,<<"самого"/utf8>>,<<"самому"/utf8>>,<<"сейчас"/utf8>>,<<"сказал"/utf8>>,<<"совсем"/utf8>>,<<"теперь"/utf8>>,<<"только"/utf8>>,<<"хорошо"/utf8>>]);
is_stopword(_, _) ->
    false.
