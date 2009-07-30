%% $Id$
%%%-------------------------------------------------------------------
%%% File    : stream.erl
%%% Author  : Christopher Faulet <christopher.faulet@capflam.org>
%%% Description :
%%%
%%% Created : 27 Oct 2005 by Christopher Faulet <christopher.faulet@capflam.org>
%%%-------------------------------------------------------------------

%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%%
%% @author Christopher Faulet <christopher.faulet@capflam.org>
%%   [http://www.capflam.org]
%% @version 1.1
%% @end
%% =====================================================================

%% @doc Stream Processing Functions.
%%
%% <p>This module contains functions for binary string (called
%% <b>stream</b>) processing.</p>
-module(stream).

-author('christopher.faulet@capflam.org').
-vsn('1.1').

%% API
-export([len/1,equal/2,concat/2,chr/2,rchr/2,str/2,rstr/2,span/2,cspan/2,
         substr/2,substr/3,tokens/2,chars/2,chars/3,copies/2,words/1,words/2,
         strip/1,strip/2,strip/3,index/2,sub_word/2,sub_word/3,left/2,left/3,
         right/2,right/3,sub_stream/2,sub_stream/3,centre/2,centre/3,
         reverse/1,member/2,to_lower/1,to_upper/1]).

-type direction() :: 'left' | 'right' | 'both'.

%%====================================================================
%% API
%%====================================================================

%% @spec len(Stream) -> Length
%%
%% @doc Returns the number of characters in the stream.
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Length = integer()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
-spec len(binary()) -> non_neg_integer().

len(Stream) when is_binary(Stream) -> size(Stream).


%% @spec equal(Stream::Stream1, Stream::Stream2) -> bool()
%%
%% @doc Tests whether two streams are equal.
%% Returns <CODE>true</CODE> if they are, otherwise <CODE>false</CODE>.
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream1 = Stream2 = binary()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
-spec equal(binary(), binary()) -> bool().

equal(Stream, Stream) when is_binary(Stream) -> true;
equal(_, _) -> false.


%% @spec concat(Stream1, Stream2) -> Stream3
%%
%% @doc Concatenates two streams to form a new stream.
%% Returns the new stream.
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream1 = Stream2 = Stream3 = binary()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
-spec concat(binary(), binary()) -> binary().

concat(Stream1, Stream2) ->
    <<Stream1/binary, Stream2/binary>>.


%% @spec chr(Stream, Character) -> Index
%%
%% @doc Returns the index of the first occurrence of <CODE>Character</CODE> in
%% <CODE>Stream</CODE>.  0 is returned if <CODE>Character</CODE> does not occur.
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Character = char()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Index = integer()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
-spec chr(binary(), char()) -> non_neg_integer().

chr(Stream, Character) when is_integer(Character) -> chr(Stream, Character, 1).

chr(<<C:8, _/binary>>, C, Offset) -> Offset;
chr(<<_:8, Rest/binary>>, C, Offset) -> chr(Rest, C, Offset+1);
chr(<<>>, _C, _I) -> 0.


%% @spec rchr(Stream, Character) -> Index
%%
%% @doc Returns the index of the last occurrence of <CODE>Character</CODE> in
%% <CODE>Stream</CODE>.  0 is returned if <CODE>Character</CODE> does not occur.
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Character = char()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Index = integer()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
-spec rchr(binary(), char()) -> non_neg_integer().

rchr(Stream, Character) when is_integer(Character) ->
    rchr(Stream, Character, 1, 0).

rchr(<<C:8, Rest/binary>>, C, Offset, _L) -> rchr(Rest, C, Offset+1, Offset);
rchr(<<_:8, Rest/binary>>, C, Offset, L) -> rchr(Rest, C, Offset+1, L);
rchr(<<>>, _C, _Offset, L) -> L.


%% @spec str(Stream, SubStream) -> Index
%%
%% @doc Returns the position where the first occurrence of
%% <CODE>SubStream</CODE> begins in <CODE>Stream</CODE>.  0 is returned if
%% <CODE>Substream</CODE> does not exist in <CODE>Stream</CODE>.
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = SubStream = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Index = integer()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
%% <DIV CLASS="REFBODY">
%% For example:
%% <P>
%% <PRE>
%% &gt; stream:str(&lt;&lt;" Hello Hello World World "&gt;&gt;, &lt;&lt;"Hello World"&gt;&gt;).
%% 8
%% </PRE>
%% </P>
%% </DIV>
%%
-spec str(binary(), binary()) -> non_neg_integer().

str(Stream, SubStream) when is_binary(SubStream) -> str(Stream, SubStream, 1).

str(<<C:8, Rest1/binary>>, <<C:8, Rest2/binary>>, Offset) ->
    case prefix(Rest2, Rest1) of
        true -> Offset;
        false -> str(Rest1, <<C:8, Rest2/binary>>, Offset+1)
    end;
str(<<_:8, Rest1/binary>>, Sub, Offset) ->
    str(Rest1, Sub, Offset+1);
str(<<>>, _Sub, _Offset) ->
    0.


%% @spec rstr(Stream, SubStream) -> Index
%%
%% @doc Returns the position where the last occurrence of <CODE>SubStream</CODE>
%% begins in <CODE>Stream</CODE>.  0 is returned if <CODE>SubStream</CODE> does
%% not exist in <CODE>Stream</CODE>.
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = SubStream = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Index = integer()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
%% <DIV CLASS="REFBODY">
%% For example:
%% <P>
%% <PRE>
%% &gt; stream:rstr(&lt;&lt;" Hello Hello World World "&gt;&gt;, &lt;&lt;"World"&gt;&gt;).
%% 20
%% </PRE>
%% </P>
%% </DIV>
%%
-spec rstr(binary(), binary()) -> non_neg_integer().

rstr(Stream, SubStream) when is_binary(SubStream) ->
    rstr(Stream, SubStream, 1, 0).

rstr(<<C:8, Rest1/binary>>, <<C:8, Rest2/binary>>, Offset, L) ->
    case prefix(Rest2, Rest1) of
        true -> rstr(Rest1, <<C:8, Rest2/binary>>, Offset+1, Offset);
        false -> rstr(Rest1, <<C:8, Rest2/binary>>, Offset+1, L)
    end;
rstr(<<_:8, Rest1/binary>>, Sub, Offset, L) ->
    rstr(Rest1, Sub, Offset+1, L);
rstr(<<>>, _Sub, _Offset, L) ->
    L.


prefix(Pre, String) ->
    Len=len(Pre),
    case String of
        <<Pre:Len/binary, _/binary>> -> true;
        _ -> false
    end.


%% @spec index(Stream, SubStream) -> Index
%% @deprecated See {@link str/2} for details.
%% @equiv str(Stream, SubStream)
-spec index(binary(), binary()) -> non_neg_integer().

index(Stream, SubStream) -> str(Stream, SubStream).


%% @spec span(Stream, Chars) -> Length
%%
%% @doc Returns the length of the maximum initial segment of
%% <CODE>Stream</CODE>, which consists entirely of bytes from
%% <CODE>Chars</CODE>.
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = Chars = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Length = integer()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
%% <DIV CLASS="REFBODY">
%% For example:
%% <P>
%% <PRE>
%% &gt; stream:span(&lt;&lt;"\t    abcdef"&gt;&gt;, &lt;&lt;" \t"&gt;&gt;).
%% 5
%% </PRE>
%% </P>
%% </DIV>
-spec span(binary(), binary()) -> non_neg_integer().

span(Stream, Chars) when is_binary(Chars) -> span(Stream, Chars, 0).

span(<<C:8, Rest/binary>>, Cs, Len) ->
    case member(C, Cs) of
        true -> span(Rest, Cs, Len+1);
        false -> Len
    end;
span(<<>>, _Cs, Len) ->
    Len.



%% @spec cspan(Stream, Chars) -> Length
%%
%% @doc Returns the length of the maximum initial segment of
%% <CODE>Stream</CODE>, which consists entirely of bytes not from
%% <CODE>Chars</CODE>.
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = Chars = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Length = integer()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
%% <DIV CLASS="REFBODY">
%% For example:
%% <P>
%% <PRE>
%% &gt; stream:cspan(&lt;&lt;"\t    abcdef"&gt;&gt;, &lt;&lt;" \t"&gt;&gt;).
%% 0
%% </PRE>
%% </P>
%% </DIV>
-spec cspan(binary(), binary()) -> non_neg_integer().

cspan(Stream, Chars) when is_binary(Chars) -> cspan(Stream, Chars, 0).

cspan(<<C:8, Rest/binary>>, Cs, Len) ->
    case member(C, Cs) of
        true -> Len;
        false -> cspan(Rest, Cs, Len+1)
    end;
cspan(<<>>, _Cs, Len) ->
    Len.


%% @spec substr(Stream, Start) -> SubStream
%% @doc See {@link substr/3} for details.
-spec substr(binary(), pos_integer()) -> binary().

substr(Stream, 1) when is_binary(Stream) ->
    Stream;
substr(Stream, Start) when is_integer(Start), Start > 1 ->
    Begin=Start-1,
    <<_:Begin/binary, Tail/binary>> = Stream,
    Tail.


%% @spec substr(Stream, Start, Length) -> SubStream
%%
%% @doc Returns a substream of <CODE>Stream</CODE>, starting at the position
%% <CODE>Start</CODE>, and ending at the end of the stream or at length
%% <CODE>Length</CODE>.
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = SubStream = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Start = Length = integer()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
%% <DIV CLASS="REFBODY">
%% For example:
%% <P>
%% <PRE>
%% &gt; stream:substr(&lt;&lt;"Hello World"&gt;&gt;, 4, 5).
%% &lt;&lt;"lo Wo"&gt;&gt;
%% </PRE>
%% </P>
%% </DIV>
-spec substr(binary(), pos_integer(), non_neg_integer()) -> binary().

substr(Stream, Start, Length) when is_integer(Start), Start > 0,
                                   is_integer(Length), Length >= 0 ->
    Begin=Start-1,
    <<_:Begin/binary, B:Length/binary, _/binary>> = Stream,
    B.




%% @spec tokens(Stream, SeparatorList) -> Tokens
%%
%% @doc Returns a list of tokens in <CODE>Stream</CODE>, separated by the
%% characters in <CODE>SeparatorList</CODE>.
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = Seps = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Tockes = [binary()]</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
%% <DIV CLASS="REFBODY">
%% For example:
%% <P>
%% <PRE>
%% &gt; stream:tokens(&lt;&lt;"abc defxxghix jkl"&gt;&gt;, &lt;&lt;"x "&gt;&gt;).
%% [&lt;&lt;"abc"&gt;&gt;, &lt;&lt;"def"&gt;&gt;, &lt;&lt;"ghi"&gt;&gt;, &lt;&lt;"jkl"&gt;&gt;]
%% </PRE>
%% </P>
%% </DIV>
-spec tokens(binary(), binary()) -> [binary(),...].

tokens(Stream, SeparatorList) when is_binary(SeparatorList) ->
    tokens1(Stream, SeparatorList, []).

tokens1(<<C:8, Rest/binary>>, Seps, Toks) ->
    case member(C, Seps) of
        true -> tokens1(Rest, Seps, Toks);
        false -> tokens2(Rest, Seps, Toks, <<C>>)
    end;
tokens1(<<>>, _Seps, Toks) ->
    lists:reverse(Toks).

tokens2(<<C:8, Rest/binary>>, Seps, Toks, Cs) ->
    case member(C, Seps) of
        true -> tokens1(Rest, Seps, [Cs|Toks]);
        false -> tokens2(Rest, Seps, Toks, <<Cs/binary, C>>)
    end;
tokens2(<<>>, _Seps, Toks, Cs) ->
    lists:reverse([Cs|Toks]).


%% @spec chars(Character, Number) -> Stream
%% @doc See {@link chars/3} for details.
-spec chars(char(), non_neg_integer()) -> binary().

chars(Character, Number) -> chars(Character, Number, <<>>).


%% @spec chars(Character, Number, Tail) -> Stream
%%
%% @doc Returns a stream consisting of <CODE>Number</CODE> of characters
%% <CODE>Character</CODE>.  Optionally, the stream can end with the stream
%% <CODE>Tail</CODE>.
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Character = char()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Tail = Stream = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Number = integer()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
-spec chars(char(), non_neg_integer(), binary()) -> binary().

chars(Character, Number, Tail) when is_integer(Number), Number > 0 ->
    chars(Character, Number-1, <<Character:8, Tail/binary>>);
chars(_C, 0, Tail) when is_integer(_C), is_binary(Tail) ->
    Tail.


%% @spec copies(Stream, Number) -> Copies
%%
%% @doc Returns a stream containing <CODE>Stream</CODE> repeated
%% <CODE>Number</CODE> times.
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = Copies = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Number = integer()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
-spec copies(binary(), non_neg_integer()) -> binary().

copies(Stream, Number) when is_binary(Stream), is_integer(Number),
                            Number >= 0 ->
    copies(Stream, Number, <<>>).

copies(_S, 0, Res)  -> Res;
copies(S, Num, Res) -> copies(S, Num-1, <<S/binary, Res/binary>>).



%% @spec words(Stream) -> Count
%% @doc See {@link words/2} for details.
-spec words(binary()) -> pos_integer().

words(Stream) -> words(Stream, $\s).


%% @spec words(Stream, Character) -> Count
%%
%% @doc Returns the number of words in <CODE>Stream</CODE>, separated by blanks
%% or <CODE>Character</CODE>.
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Character = char()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
%% <DIV CLASS="REFBODY">
%% For example:
%% <P>
%% <PRE>
%% &gt; stream:words(&lt;&lt;" Hello old boy!"&gt;&gt;, $o).
%% 4
%% </PRE>
%% </P>
%% </DIV>
-spec words(binary(), char()) -> pos_integer().

words(Stream, Character) when is_integer(Character) ->
    w_count(strip(Stream, both, Character), Character, 0).

w_count(<<>>, _, Num) -> Num+1;
w_count(<<C:8, Rest/binary>>, C, Num) ->
    w_count(strip(Rest, left, C), C, Num+1);
w_count(<<_:8, Rest/binary>>, C, Num) ->
    w_count(Rest, C, Num).


%% @spec sub_word(Stream, Number) -> Word
%% @doc See {@link sub_word/3} for details.
-spec sub_word(binary(), non_neg_integer()) -> binary().

sub_word(Stream, Number) -> sub_word(Stream, Number, $\s).


%% @spec sub_word(Stream, Number, Character) -> Word
%%
%% @doc Returns the word in position <CODE>Number</CODE> of <CODE>Stream</CODE>.
%% Words are separated by blanks or <CODE>Characters</CODE>.
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = Word = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Character = char()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Number = integer()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
%% <DIV CLASS="REFBODY">
%% For example:
%% <P>
%% <PRE>
%% &gt; stream:sub_word(&lt;&lt;" Hello old boy!"&gt;&gt;, 3, $o).
%% &lt;&lt;"ld b"&gt;&gt;
%% </PRE>
%% </P>
%% </DIV>
-spec sub_word(binary(), non_neg_integer(), char()) -> binary().

sub_word(S, Offset, C) when is_integer(Offset), Offset >= 0 ->
    case words(S, C) of
        Num when Num < Offset -> <<>>;
        _Num -> s_word(strip(S, left, C), Offset, C, 1, <<>>)
    end.


s_word(<<>>, _, _, _, Res) -> Res;
s_word(<<C:8, _/binary>>, Offset, C, Offset, Res) -> Res;
s_word(<<H:8, T/binary>>, Offset, C, Offset, Res) ->
    s_word(T, Offset, C, Offset, <<Res/binary, H:8>>);
s_word(<<C:8, T/binary>>, Stop, C, Offset, Res) when Offset < Stop ->
    s_word(strip(T, left, C), Stop, C, Offset+1, Res);
s_word(<<_:8, T/binary>>, Stop, C, Offset, Res) when Offset < Stop ->
    s_word(T, Stop, C, Offset, Res).




%% @spec strip(Stream) -> Stripped
%% @doc See {@link strip/3} for details.
-spec strip(binary()) -> binary().

strip(Stream) -> strip(Stream, both).

%% @spec strip(Stream, Direction) -> Stripped
%% @doc See {@link strip/3} for details.
-spec strip(binary(), direction()) -> binary().

strip(Stream, left)  -> strip_left(Stream, $\s);
strip(Stream, right) -> strip_right(Stream, $\s);
strip(Stream, both)  -> strip_right(strip_left(Stream, $\s), $\s).



%% @spec strip(Stream, Direction, Character) -> Stripped
%%
%% @doc Returns a stream, where leading and/or trailing blanks or a number of
%% <CODE>Character</CODE> have been removed.  <CODE>Direction</CODE> can be
%% <CODE>left</CODE>, <CODE>right</CODE>, or <CODE>both</CODE> and indicates
%% from which direction blanks are to be removed.  The function
%% <CODE>strip/1</CODE> is equivalent to <CODE>strip(String, both)</CODE>.
%%
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = Stripped = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Direction = left | right | both</CODE></STRONG><BR/>
%%       <STRONG><CODE>Character = char()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
%% <DIV CLASS="REFBODY">
%% For example:
%% <P>
%% <PRE>
%% &gt; stream:strip(&lt;&lt;"...Hello....."&gt;&gt;, both, $.).
%% &lt;&lt;"Hello"&gt;&gt;
%% </PRE>
%% </P>
%% </DIV>
-spec strip(binary(), direction(), char()) -> binary().

strip(S, right, Char) when is_integer(Char) -> strip_right(S, Char);
strip(S, left,  Char) when is_integer(Char) -> strip_left(S, Char);
strip(S, both,  Char) when is_integer(Char) -> strip_right(strip_left(S, Char), Char).


strip_left(<<C:8, Rest/binary>>, C) -> strip_left(Rest, C);
strip_left(<<>>, _C) -> <<>>;
strip_left(S, _C) when is_binary(S) -> S.

strip_right(<<C:8, Rest/binary>>, C) ->
    case strip_right(Rest, C) of
        <<>> -> <<>>;
        S -> <<C:8, S/binary>>
                 end;
strip_right(<<>>, _C) -> <<>>;
strip_right(<<H:8, T/binary>>, C) ->
    S = strip_right(T, C),
    <<H:8, S/binary>>.


%% @spec left(Stream, Number) -> Left
%% @doc See {@link left/3} for details.
-spec left(binary(), non_neg_integer()) -> binary().

left(Stream, Number) -> left(Stream, Number, $\s).

%% @spec left(Stream, Number, Character) -> Left
%%
%% @doc Returns the <CODE>Stream</CODE> with the length adjusted in accordance
%% with <CODE>Number</CODE>.  The left margin is fixed. If the
%% length(<CODE>Stream</CODE>) &lt; <CODE>Number</CODE>, <CODE>Stream</CODE> is
%% padded with blanks or <CODE>Characters</CODE>.
%%
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = Left = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Number = integer()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Character = char()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
%% <DIV CLASS="REFBODY">
%% For example:
%% <P>
%% <PRE>
%% &gt; stream:left(&lt;&lt;"Hello"&gt;&gt;, 10, $.).
%% &lt;&lt;"Hello....."&gt;&gt;
%% </PRE>
%% </P>
%% </DIV>
-spec left(binary(), non_neg_integer(), char()) -> binary().

left(Stream, Number, Character) when is_integer(Number), Number >= 0,
                                     is_integer(Character) ->
    Slen = len(Stream),
    if
        Slen > Number -> substr(Stream, 1, Number);
        Slen < Number -> l_pad(Stream, Number-Slen, Character);
        Slen == Number -> Stream
    end.

l_pad(S, Num, C) ->
    T =  chars(C, Num),
    <<S/binary, T/binary>>.


%% @spec right(Stream, Number) -> Right
%% @doc See {@link right/3} for details.
-spec right(binary(), non_neg_integer()) -> binary().

right(Stream, Number) -> right(Stream, Number, $\s).

%% @spec right(Stream, Number, Character) -> Right
%%
%% @doc Returns the <CODE>Stream</CODE> with the length adjusted in accordance
%% with <CODE>Number</CODE>.  The right margin is fixed. If the
%% length(<CODE>Stream</CODE>) &lt; <CODE>Number</CODE>, <CODE>Stream</CODE> is
%% padded with blanks or <CODE>Characters</CODE>.
%%
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = Right = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Number = integer()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Character = char()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
%% <DIV CLASS="REFBODY">
%% For example:
%% <P>
%% <PRE>
%% &gt; stream:right(&lt;&lt;"Hello"&gt;&gt;, 10, $.).
%% &lt;&lt;".....Hello"&gt;&gt;
%% </PRE>
%% </P>
%% </DIV>
-spec right(binary(), non_neg_integer(), char()) -> binary().

right(Stream, Number, Character) when is_integer(Number), Number >= 0,
                                      is_integer(Character) ->
    Slen = len(Stream),
    if
        Slen > Number -> substr(Stream, Slen-Number+1);
        Slen < Number -> r_pad(Stream, Number-Slen, Character);
        Slen == Number -> Stream
    end.

r_pad(S, Num, C) -> chars(C, Num, S).


%% @spec centre(Stream, Number) -> Center
%% @doc See {@link center/3} for details.
-spec centre(binary(), non_neg_integer()) -> binary().

centre(Stream, Number) -> centre(Stream, Number, $\s).


%% @spec centre(Stream, Number, Character) -> Center
%%
%% @doc Returns a stream, where <CODE>Stream</CODE> is centred in the stream and
%% surrounded by blanks or <CODE>Characters</CODE>.  The resulting stream will
%% have the length <CODE>Number</CODE>.
%%
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = Center = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Number = integer()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Character = char()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
-spec centre(binary(), non_neg_integer(), char()) -> binary().

centre(Stream, Number, Character) when is_integer(Number), Number >= 0,
                                       is_integer(Character) ->
    Slen = len(Stream),
    if
        Slen > Number -> substr(Stream, (Slen-Number) div 2 + 1, Number);
        Slen < Number ->
            N = (Number-Slen) div 2,
            r_pad(l_pad(Stream, Number-(Slen+N), Character), N, Character);
        Slen == Number -> Stream
    end;
centre(_S, 0, _C) when is_binary(_S), is_integer(_C) -> %%Strange cases to centre string
    <<>>.


%% @spec sub_stream(Stream, Start) -> SubStream
%% @doc See {@link sub_stream/3} for details.
-spec sub_stream(binary(), pos_integer()) -> binary().

sub_stream(Stream, Start) -> substr(Stream, Start).


%% @spec sub_stream(Stream, Start, Stop) -> SubStream
%%
%% @doc Returns a substream of <CODE>Stream</CODE>, starting at the position
%% <CODE>Start</CODE> to the end of the stream, or to and including the
%% <CODE>Stop</CODE> position.
%%
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = SubStream = binary()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Start = Stop = integer()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
%% <DIV CLASS="REFBODY">
%% For example:
%% <P>
%% <PRE>
%% &gt; stream:sub_stream(&lt;&lt;"Hello World"&gt;&gt;, 4, 8).
%% &lt;&lt;"lo Wo"&gt;&gt;
%% </PRE>
%% </P>
%% </DIV>
-spec sub_stream(binary(), pos_integer(), pos_integer()) -> binary().

sub_stream(Stream, Start, Stop) -> substr(Stream, Start, Stop - Start + 1).


%% @spec member(Character, Stream) -> bool()
%%
%% @doc Returns <CODE>true</CODE> if <CODE>Character</CODE> is contained in the
%% binary <CODE>Stream</CODE>, otherwise <CODE>false</CODE>.
%%
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Character = char()</CODE></STRONG><BR/>
%%       <STRONG><CODE>Stream = binary()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
-spec member(char(), binary()) -> bool().

member(Character, <<Character:8, _/binary>>) -> true;
member(Character, <<_:8, Rest/binary>>) -> member(Character, Rest);
member(_C, <<>>) when is_integer(_C) -> false.


%% @spec reverse(Stream) -> Reversed
%%
%% @doc Returns a binary with the top level bytes in <CODE>Stream</CODE> in
%% reverse order.
%%
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = Reversed = binary()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
-spec reverse(binary()) -> binary().

reverse(Stream) -> reverse(Stream, <<>>).

reverse(<<>>, Rest) -> Rest;
reverse(<<H:8, T/binary>>, Rest) -> reverse(T, <<H:8, Rest/binary>>).


%% @spec to_lower(Stream) -> Result
%%
%% @doc The given stream is converted to lower case. Note that the supported
%% character set is ISO/IEC 8859-1 (a.k.a. Latin 1), all values outside this set
%% is unchanged
%%
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = Result = binary()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
-spec to_lower(binary()) -> binary().

to_lower(Stream) when is_list(Stream) ->
    << <<(to_lower_char(C))>> || <<C>> <= Stream>>.

to_lower_char(C) when is_integer(C), $A =< C, C =< $Z -> C + 32;
to_lower_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 -> C + 32;
to_lower_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE -> C + 32;
to_lower_char(C) -> C.


%% @spec to_upper(Stream) -> Result
%%
%% @doc The given stream is converted to upper case. Note that the supported
%% character set is ISO/IEC 8859-1 (a.k.a. Latin 1), all values outside this set
%% is unchanged
%%
%%
%% <DIV CLASS="REFBODY"><P>Types:</P>
%%   <DIV CLASS="REFTYPES">
%%     <P>
%%       <STRONG><CODE>Stream = Result = binary()</CODE></STRONG><BR/>
%%     </P>
%%   </DIV>
%% </DIV>
%%
-spec to_upper(binary()) -> binary().

to_upper(Stream) when is_binary(Stream) ->
    << <<(to_upper_char(C))>> || <<C>> <= Stream>>.


to_upper_char(C) when is_integer(C), $a =< C, C =< $z -> C - 32;
to_upper_char(C) when is_integer(C), 16#E0 =< C, C =< 16#F6 -> C - 32;
to_upper_char(C) when is_integer(C), 16#F8 =< C, C =< 16#FE -> C - 32;
to_upper_char(C) -> C.
