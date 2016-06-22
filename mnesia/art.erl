-module(art).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl").

-record(painting, {index, artist, title}).

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(painting,
        [ {disc_copies, [node()] },
             {attributes,
                record_info(fields,painting)} ]).

insert( Index, Artist, Title) ->
    Fun = fun() ->
         mnesia:write(
         #painting{ index=Index,
                   artist=Artist,
                        title=Title    } )
               end,
         mnesia:transaction(Fun).

select( Index) ->
    Fun =
        fun() ->
            mnesia:read({painting, Index})
        end,
    {atomic, [Row]}=mnesia:transaction(Fun),
    io:format(" ~p ~p ~n ", [Row#painting.artist, Row#painting.title] ).

select_some( Artist) ->
    Fun =
        fun() ->
            mnesia:match_object({painting, '_', Artist, '_' } )
        end,
    {atomic, Results} = mnesia:transaction( Fun),
    Results.

select_all() ->
    mnesia:transaction(
    fun() ->
        qlc:eval( qlc:q(
            [ X || X <- mnesia:table(painting) ]
        ))
    end ).

select_search( Word ) ->
    mnesia:transaction(
    fun() ->
         qlc:eval( qlc:q(
              [ {F0,F1,F2,F3} ||
                   {F0,F1,F2,F3} <-
                        mnesia:table(painting),
                        (string:str(F2, Word)>0) or
                        (string:str(F3, Word)>0)
               ] ))
    end ).

%   Sample output:

%   6> c(art).
%   {ok,art}

%   7> art:init().
%   {atomic,ok}

%   6> art:insert(1,"Dali","The Ghost of Vermeer").
%   {atomic,ok}

%   7> art:select(1).
%   "Dali" "The Ghost of Vermeer"
%   ok

%   8> art:insert(2,"Dali","The Persistence of Memory").
%   {atomic,ok}

%   9> art:select(2).
%   "Dali" "The Persistence of Memory"
%   ok

%   10> art:select(1).
%   "Dali" "The Ghost of Vermeer"
%   ok

%   25> art:insert(3,"Vermeer", "Girl With Pearl Earring").
%   {atomic,ok}

%   26> art:select_some("Dali").
%   [{painting,1,"Dali","The Ghost of Vermeer"},
%   {painting,2,"Dali","The Persistence of Memory"}]

%   27> art:select_all().
%   {atomic,[{painting,1,"Dali","The Ghost of Vermeer"},
%               {painting,2,"Dali","The Persistence of Memory"},
%               {painting,3,"Vermeer","Girl With Pearl Earring"}]}

%---to run a new session after restarting erlang---

%   2> art:init().
%   {aborted,{already_exists,painting}}

%   3> art:select_search("Vermeer").
%   {atomic,[{painting,1,"Dali","The Ghost of Vermeer"},
%         {painting,3,"Vermeer","Girl With Pearl Earring"}]}
