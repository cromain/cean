-module(besea).

%% =====================================================================
%% Bengts Erlang Self Extracting Archive
%%
%% Copyright (C) 2007 Bengt Kleberg
%%
%% This progam is free software; you can redistribute it and/or modify
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
%% Author contact: bengt.kleberg@comhem.se
%%
%% ===============================================================%%
%% @doc Handle a self extracting Erlang archive.
%%
%%%		<p>This escript makes it possible to create an Erlang
%%%		self extracting archive. The archive can list its contents and
%%%		extract its contents. it is also possible to run Erlang
%%%		modules in the archive, without first extracting them.</p>

-export([main/1]).


%% @spec main(Arguments::list()) -> item()
% input
%	Arguments : list() of list().
% returns
%	
% execptions
%
% main() is called from escript and has a single list of lists as its
% argument. These are the scripts arguments.

main( Args ) ->
	Script = script_name(),
	case Args of
	["extract"|Wanted] -> extract( Script, Wanted );
	["help"] -> help( Script );
	["help", Command] -> help( Script, Command );
	["list"|Contents] -> list( Script, Contents );
	["new", File|Contents] -> new( Script, File, Contents );
	["run", Module, Function|Arguments] -> run( Script, Module, Function, Arguments );
	_ -> help( Script )
	end.

%% @spec binary_part_starts(Binary::binary(), Part::binary()) -> integer()
% input
%	Binary : any binary (here used with file contents)
%	Part : any binary (here used with a string expected in the file)
% returns
%	-1 if Part is <<>> or not found, 0 -> size(Binary) if found.
%
% execption
%
% find out the offset (to be used by erlang:split_binary/2)
% where a binary Part starts in Binary.

binary_part_starts( Binary, Part ) -> binary_part_starts( Binary, Part, 0, Part, 0 ).

binary_part_starts( _Binary, <<>>, Found, _Whole_part, _N ) -> Found;
binary_part_starts( <<>>, _Part, _Found, _Whole_part, _N ) -> -1;
binary_part_starts( <<B:1/binary,Tb/binary>>, <<B:1/binary,Tp/binary>>, Found, Whole_part, N ) ->
	binary_part_starts( Tb, Tp, Found, Whole_part, N+1 );
binary_part_starts( <<_B:1/binary,Tb/binary>>, _Part, _Found, Whole_part, N ) ->
	binary_part_starts( Tb, Whole_part, N+1, Whole_part, N+1 ).


%% @spec code(File::string()) -> binary()
% input
%	File : a file name
% returns
%	the contents of File, before the separator between code and data
% execption
%
% find the code (script) part of a File

code( File ) ->
	{ok, File_contents} = file:read_file( File ),
	{Code, _Rest} = split_file( File_contents, end_of_code() ),
	Code.


%% @spec data(File::string()) -> binary()
% input
%	File : a file name
% returns
%	the contents of File, after the separator between code and data
% execption
%
% find the data part of a File

data( Script ) ->
	{ok, File_contents} = file:read_file( Script ),
	{_Code, Data} = split_file( File_contents, end_of_code() ),
	erlang:list_to_binary( uncomment(Data) ).

% constants
data_size_binary() -> 20.
data_size_printable_binary() -> data_size_binary() * printable_binary_octet_size().

end_of_code() ->
	erlang:list_to_binary( [line_start(), <<"End of Code. Beginning of Data (if any).">>, line_end()] ).

%% @spec extract(Script::string(), Wanted_list::list()) -> item()
% input
%	Script : the file name of his script
%	Wanted_list : list() of file names that we want to extract
% returns
%
% execption
%
% extract all files in the archive (this script) if Wanted_list is []
% or
% extract only files in Wanted_list from the archive
% 
extract( Script, Wanted_list ) ->
	Zip_archive = data( Script ),
	case Wanted_list of
	[] -> zip:extract( Zip_archive );
	_Wanted -> extract( Script, Wanted_list, Zip_archive )
	end.

extract( _Script, Wanted_list, Zip_archive ) ->
	Fun = fun (File_info) -> is_file_wanted( Wanted_list, File_info ) end,
	zip:extract( Zip_archive, [{file_filter, Fun}] ).

	
help( Script ) ->
	io:fwrite( "The commands are: extract help list new run~n" ),
	io:fwrite( "Do:~n~s help <command>~n", [Script] ),
	io:fwrite( "for more help~n" ).

help( Script, "extract" ) ->
	io:fwrite( "~s extract~n", [Script] ),
	io:fwrite( "Extract the contents of this script archive.~n" );
help( Script, "help" ) ->
	help( Script );
help( Script, "list" ) ->
	io:fwrite( "~s list~n", [Script] ),
	io:fwrite( "List the contents of this script archive.~n" ),
	io:fwrite( "~s list <file1> ... <fileN>~n", [Script] ),
	io:fwrite( "List <file1> to <fileN> in the archive. Warn if they do not exist.~n" ),
	io:fwrite( "If any of <file1> to <fileN> is a directory all files under it will be recursively listed.~n" );
help( Script, "new" ) ->
	io:fwrite( "~s new <filename>~n", [Script] ),
	io:fwrite( "Create a new copy of the code of this script called <filename>~n" ),
	io:fwrite( "~s new <filename> <file1> ... <fileN>~n", [Script] ),
	io:fwrite( "Create a new copy of of the code of this script called <filename> archiving <file1> to <fileN> into it~n" ),
	io:fwrite( "If any of <file1> to <fileN> is a directory all files under it will be recursively added to the archive.~n" );
help( Script, "run" ) ->
	io:fwrite( "~s run <module> <function> [<arg1> ...]~n", [Script] ),
	io:fwrite( "Run module:function( Arg1, ... ), where all arguments are strings~n" ),
	io:fwrite( "Module and function are in this script archive~n" ),
	io:fwrite( "Without any arguments you will get module:function()~n" );
help( Script, Command ) ->
	io:fwrite( "Unknown command ~s~n", [Command] ),
	help( Script ).


%% @spec is_file_wanted(Wanted_list::list(), File_info::tuple()) -> boolean()
% input
%	Wanted_list : list() of file names that we want
%	File_info : tuple from Zip archive. Only the File_name list is of interest
% returns
%	boolean()
% execption
%
% find out if the File_name in the Zip archive tuple, is one that we want.
% ie, if is present in the Wanted_list.
% Wanted_list can be the name of a directory, and then we want all files in that directory.

is_file_wanted( Wanted_list,
		{_File_info, File_name, _Other_info, _Comment, _Size, _Other_size} ) ->
	Fun = fun(Wanted) ->
		case Wanted of
		File_name -> true;
		_Else ->
			%% no direct match with Wanted and File_name.
			%% it is also possible that Wanted is a directory path
			%% in the begining of File_anme
			%% if not present add '/' to the end of Wanted
			Directory = case lists:reverse( Wanted ) of
				[$/|_T] -> Wanted;
				Reversed -> lists:reverse( [$/|Reversed] )
				end,
			case string:str( File_name, Directory ) of
			1 -> true;
			_N -> false
			end
		end
	end,
	lists:any( Fun, Wanted_list ).

% constants

line_end() -> <<"\n">>.
line_end_size() -> erlang:size( line_end() ).
line_start() -> <<"% ">>.
line_start_size() -> erlang:size( line_start() ).

%% @spec list(Script::string(), Contents::list()) -> item()
% input
%	Script : the file name of his script
%	Contents : list() of file names that we want to list
% returns
%
% execption
%
% list all the contents of this Script/archive if Wanted_list is []
% or
% list only files in Wanted_list from the archive

list( Script, Contents ) ->
	Zip_archive = data( Script ),
	case Contents of
	[] -> zip:t( Zip_archive );
	Contents -> list( Script, Contents, zip:list_dir(Zip_archive) )
	end.

list( Script, Contents, {ok, [_Comment|Files]} ) ->
	Fun = fun({_File_info, File_name, _Other_info, _Comment1, _Size, _Other_size}) -> io:fwrite( "~s~n", [File_name] ) end,
	lists:foreach( Fun, wanted_files(Script, Contents, Files) );
list( Script, _Contents, {error, _Reason} ) ->
	io:fwrite( "~s: error opening archive~n", [Script] ).

%% @spec new(Script::string(), Name::list(), Files::list()) -> item()
% input
%	Script : the file name of his script
%	Name : the file name of the new script
%	Files : list() of file names that we want to put into File
% returns
%	
% execption
%
% Create a new script called File using this script as a template.
% all files in Contents will be placed in the new archive.
% if contents is [] the new script/archive will be empty

new( Script, Name, Files ) ->
	Code = code( Script ),
	Comment_data = new_comment_data( Script, Name, Files ),
	New = [Code, end_of_code()|Comment_data],
	file:write_file( Name, erlang:list_to_binary(New) ).

%% @spec new_comment_data(Script::string(), Name::list(), Files::list()) -> binary()
% input
%	Script : the file name of his script
%	Name : the file name of the new script
%	Files : list() of file names that we want to put into File
% returns
%	The contents of files in a zip archive, as lots of commented lines
% execption
%
% Take the Files and but them into as ziop archive.,
% turn the zip archive into commented (starts with %) lines
% these are suitable to have as data part in an escript

new_comment_data( _Script, _Name, [] ) -> [<<>>];
new_comment_data( Script, Name, Files ) ->
	case zip_create( Name, Files ) of
	Zip_archive when is_binary(Zip_archive) -> printable_binary_comment_lines( Zip_archive );
	Error ->
		io:fwrite( "~s: could not create archive: ~w~n", [Script, Error] ),
		[<<>>]
	end.

%% @spec printable_binary_comment_lines(Binary::binary()) -> binary()
% input
%	Binary : any binary (in this case a zip archive)
% returns
%	the binary as a lot of commented (starts with %) lines
% execption
%
% take a binary and change it into lots of lines of hex numbers.
% all lines start with % and are suitable commnts in an escript

printable_binary_comment_lines( Binary ) ->
        printable_binary_comment_lines( Binary, data_size_binary(), line_start(), line_end(), [] ).

printable_binary_comment_lines( Binary, Size, Start, End, Acc ) ->
	case Binary of
	<<Chunk:Size/binary, T/binary>> ->
        	Commented_line = [Start, printable_binary(Chunk), End],
        	printable_binary_comment_lines( T, Size, Start, End, [Commented_line|Acc] );
	<<_Empty:0, Last/binary>> ->
        	Commented_line = [Start, printable_binary(Last), End],
        	lists:reverse( [Commented_line|Acc] )
	end.

printable_binary( <<>> ) -> [];
printable_binary( <<Octet:1/binary, T/binary>> ) ->
        [printable_binary_octet(Octet)|printable_binary(T)].
% no more binary since we have less than 8 bits
printable_binary_octet( <<High:4, Low:4>> ) ->
        [printable_binary_4bits( High ), printable_binary_4bits( Low )].

printable_binary_octet_size() -> 2.

printable_binary_4bits( 0 ) -> <<"0">>;
printable_binary_4bits( 1 ) -> <<"1">>;
printable_binary_4bits( 2 ) -> <<"2">>;
printable_binary_4bits( 3 ) -> <<"3">>;
printable_binary_4bits( 4 ) -> <<"4">>;
printable_binary_4bits( 5 ) -> <<"5">>;
printable_binary_4bits( 6 ) -> <<"6">>;
printable_binary_4bits( 7 ) -> <<"7">>;
printable_binary_4bits( 8 ) -> <<"8">>;
printable_binary_4bits( 9 ) -> <<"9">>;
printable_binary_4bits( 10 ) -> <<"a">>;
printable_binary_4bits( 11 ) -> <<"b">>;
printable_binary_4bits( 12 ) -> <<"c">>;
printable_binary_4bits( 13 ) -> <<"d">>;
printable_binary_4bits( 14 ) -> <<"e">>;
printable_binary_4bits( 15 ) -> <<"f">>.



%% @spec run(Script::string(), Module::list(), Function::string(), Arguments::list()) -> item()
% input
%	Script : the name of this script/archive
%	Module : the name of the module to run
%	Function : the name of the function to run
%	Arguments : a list of arguments (all strings) to the function
% returns
%	
% execption
%
% load all erlang byte code files from the archive.
% run module functiton with arguments

run( Script, Module, Function, Arguments ) ->
	Data = data( Script ),
	run( Script, Module, Function, Arguments, zip:extract( Data, [memory]) ).

run( Script, Module, Function, Arguments, {ok, Files} ) ->
	Extension = code:objfile_extension(),
	Fun = fun({File_name, File_contents}) ->
		case filename:extension( File_name ) of
		Extension ->
			M = erlang:list_to_atom( filename:basename(File_name, Extension) ),
			case code:load_binary( M, File_name, File_contents ) of
			{module, M} -> ok;
			Error -> io:fwrite( "~s: failed to load ~s: ~p~n", [Script, File_name, Error] )
			end;
		_Else -> ok
		end 
	end,
	lists:foreach( Fun, Files ),
	M = erlang:list_to_atom( Module ),
	F = erlang:list_to_atom( Function ),
	erlang:apply( M, F, Arguments );
run( Script, _Module, _Function, _Arguments, {error, _Reason} ) ->
	io:fwrite( "~s: error extracting archive~n", [Script] ).

%% @spec script_name() -> string()
% input
%
% returns
%	the name of this script as a string.
%
% execption
%
script_name() ->
	[Script_name|_T] = init:get_plain_arguments(),
	Script_name.

%% @spec slit_file(File_contents::binary(), Separator::binary()) -> tuple()
% input
%	File_contents : file contents as binary()
%	Separator : a part of file contens, also binary()
% returns
%	a tuple of binaries. First member is the file contents before Separator
%	the second memeber is the  file contents after Separator
%
% execption
%
% split a binary into two parts. Before and after Separator.
% Separator itself is not part of before, and not first in after.
% But it might be somewhere else in after.

split_file( File_contents, Separator ) ->
	case binary_part_starts( File_contents, Separator ) of
	-1 -> {File_contents,<<>>};
	N ->	% split right before End_of_code or else the first character will be doubled
		{Code,Rest} = erlang:split_binary( File_contents, N ),
		{_Separator,Data} = erlang:split_binary( Rest, erlang:size(Separator) ),
		{Code,Data}
	end.


%% @spec uncomment(Binary::binary()) -> binary()
% input
%	Binary : lot of commented (starts with %) lines
% returns
%	the binary without commentes and lines
% execption
%
% take lots of lines of hex numbers.
% all lines start with % and are suitable comments in an escript
% change them into a binary,

uncomment( <<>> ) -> [<<>>];
uncomment( Printable_binary_comment_lines ) ->
	uncomment( Printable_binary_comment_lines, data_size_printable_binary(), line_start_size(), line_end_size(), [] ).

uncomment( Binary, Size, Start_size, End_size, Acc ) ->
	case Binary of
	<<>> -> lists:reverse( Acc );
	<<_Start:Start_size/binary, Printable_binary:Size/binary, _End:End_size/binary, T/binary>> ->
		Unprintable_binary = unprintable_binary( Printable_binary ),
		uncomment( T, Size, Start_size, End_size, [Unprintable_binary|Acc] );
	<<_Start:Start_size/binary, Last_line_end/binary>> ->
		{Last,_End} = erlang:split_binary( Last_line_end, erlang:size(Last_line_end) - End_size ),
		lists:reverse( [unprintable_binary(Last)|Acc] )
	end.


unprintable_binary( <<>> ) -> [];
unprintable_binary( <<Printable_octet:2/binary, T/binary>> ) ->
        [unprintable_binary_octet(Printable_octet)|unprintable_binary(T)].

unprintable_binary_octet( <<High:1/binary, Low:1/binary>> ) ->
	<<(unprintable_binary_4bits( High ) * 16 + unprintable_binary_4bits( Low ))>>.

unprintable_binary_4bits( <<"0">> ) -> 0;
unprintable_binary_4bits( <<"1">> ) -> 1;
unprintable_binary_4bits( <<"2">> ) -> 2;
unprintable_binary_4bits( <<"3">> ) -> 3;
unprintable_binary_4bits( <<"4">> ) -> 4;
unprintable_binary_4bits( <<"5">> ) -> 5;
unprintable_binary_4bits( <<"6">> ) -> 6;
unprintable_binary_4bits( <<"7">> ) -> 7;
unprintable_binary_4bits( <<"8">> ) -> 8;
unprintable_binary_4bits( <<"9">> ) -> 9;
unprintable_binary_4bits( <<"a">> ) -> 10;
unprintable_binary_4bits( <<"b">> ) -> 11;
unprintable_binary_4bits( <<"c">> ) -> 12;
unprintable_binary_4bits( <<"d">> ) -> 13;
unprintable_binary_4bits( <<"e">> ) -> 14;
unprintable_binary_4bits( <<"f">> ) -> 15.


%% @spec wanted_files(Script::string(), Wanted_list::list(), File_infos::list()) -> list()
% input
%	Script : the name of this script/archive
%	Wanted_list : list() of file names that we want
%	File_infos : list of tuple from Zip archive.
% returns
%	a list of the files we want form the archive
% execption
%
% find out if the File_name in the Zip archive tuple, is one that we want.
% ie, if is present in the Wanted_list.
% Wanted_list can be the name of a directory, and then we want all files in that directory.

wanted_files( Script, Wanted_list, File_infos ) ->
	Fun = fun(Wanted) ->
		case wanted_files( Wanted, File_infos ) of
		[] ->
			io:fwrite( "~s: ~s: No such file or directory in archive~n", [Script, Wanted] ),
			[];
		Found -> Found
		end
	end,
	lists:flatmap( Fun, Wanted_list ).

wanted_files( Wanted, File_infos ) ->
	Fun = fun(File_info) -> is_file_wanted( [Wanted], File_info ) end,
	lists:filter( Fun, File_infos ).


%% @spec zip_create(Name::string(), Files::list()) -> binary()
% input
%	Name : name of the zip archive we want to create
%	Files : the files we want in the zip archive
% returns
%	zip archive
% execption
%
% create a zip archive with Files in it.

zip_create( Name, Files ) ->
	Fun = fun (File) ->
		case filelib:is_dir( File ) of
		%% this is a directory. recurse over it, and its subdirectories,
		%% accumulate all files
		true -> filelib:fold_files( File, ".+", true, fun (F, Acc) -> [F|Acc] end, [] );
		false -> [File]
		end
	end,
	All_files = lists:flatmap( Fun, Files ),
	case zip:create( Name, All_files, [memory] ) of
	{ok, {Name, Zip_archive}} -> Zip_archive;
	{error, Error} -> Error
	end.

