%%% ====================================================================
%%% This software is copyright (c) 2006-2014, ProcessOne.
%%%
%%% @copyright 2006-2014 ProcessOne
%%% @author Christophe Romain <christophe.romain@process-one.net>
%%%   [http://www.process-one.net/]
%%% @author Bengt Kleberg <bengt.kleberg@ericsson.com>
%%% @version {@vsn}, {@date} {@time}
%%% @end
%%% ====================================================================

%%% @doc The module <strong>{@module}</strong> is the command line interface
%%% to CEAN site. It handles erlang packages installation/upgrade/remove.
%%%
%%% @reference See <a href="http://cean.process-one.net">CEAN Web Site</a>
%%% for detailed CEAN description.

-module(cean).
-author("Christophe Romain <christophe.romain@process-one.net>").
-author("Bengt Kleberg <bengt.kleberg@ericsson.com>").
-vsn("2.0").

%% Packaging service
-export([available/0, available/1, call/0, depends/1,
        display/1, help/0, help/1, install/1,
        install/2, installed/0, installed/1, installed_dependent_upon/1,
        is_installed/1, new/0, newest_available/1, otp_version/0,
        newest_installed/1, proxy_host/2, proxy_user/2, purge/0,
        remote/2, search/1, server/1, stop/0, uninstall/1,
        upgrade/0, upgrade/1, upgrade/2]).

-define(DEFAULT_SERVER, "cean.process-one.net").
-define(INSTALL_MAX_MINUTES, 1).
-define(UPGRADE_MAX_MINUTES, 1).
-define(MINUTE, 60000).
-define(PROXY_HOST, "HTTP_PROXY").
-define(PROXY_HOST_SEPARATOR, ":").
-define(PROXY_USER, "PROXY_USER").
-define(PROXY_USER_SEPARATOR, [4]).

%% -- public functions
% @type package = list() | atom().

available() ->
   case getlist() of
     {ok, L} ->
         %% remove version, get rid of duplicates (the same package with different versions)
         lists:usort([no_vsn(LibDir) || LibDir <- L]);
     {error, R} -> io:fwrite("error: ~p~n", [R]), []
   end.

%% @spec available(Package::package()) -> [package()].
% input
%    Package : the base name (no version) of a package
% returns
%    a list of all available versions of the package
% exceptions
%
available( Package ) when is_atom(Package) ->
   available(erlang:atom_to_list(Package));
available( Package ) when is_list(Package) ->
   case getlist() of
     {ok, L} -> select_packages( Package, L );
     {error, R} -> io:fwrite("error: ~p~n", [R]), []
   end.

%% @spec display(Item::item()) -> atom()
% input
%    Item is either an atom (one of available, installed, new) or a list of strings.
%    If Item is an atom the function with the same name as the atom is run and
%    the result (a list of strings) is displayed.
%    this function is a helper for when the result of a function is a too long list
% returns
%
% execption
%
display(available) -> display(available());
display(installed) -> display(installed());
display(new) -> display(new());
display(List) when is_list(List) ->
   lists:foreach(fun (Item) -> io:fwrite("~s~n", [Item])
                 end,
                 List),
   io:fwrite("~n").

%% @spec depends(Package::item()) -> list()
% input
%    Package can be either an atom or a string.
%    If it is an atom it will be turned into a string
% returns
%    A list of packages that package depends upon.
% execption
%
depends(Package) when is_atom(Package) ->
    depends(erlang:atom_to_list(Package));
depends( Package ) when is_list(Package) ->
    try
    lists:foldl( fun depends/2, [], [Package] )
    catch
    _:Error ->
    io:fwrite("failed to get dependencies: ~w~n", [Error]),
    []
    end.

depends( Package, Acc ) ->
    depends( Package, getdeps(Package), Acc ).

depends( _Package, {ok, Depends}, Acc ) ->
    case [X || X <- Depends, not lists:member(X, Acc)]
    of [] -> Acc
    ; New -> lists:foldl( fun depends/2, New ++ Acc, New )
    end;
depends( Package, {error, Error}, _Acc )  ->
    erlang:throw( {Package, depends, Error} ).

%% @spec help() -> atom()
% input
%
% returns
%
% execption
%
% description
%    disply help text
%
help() ->
   io:fwrite("The functions are: ~p~n",
             [exported_functions()]),
   io:fwrite("Do help(<function>) for more help on that function.~n").

%% @spec help(Function::atom()) -> atom()
% input
%    Function : a function name
%
% returns
%
% execption
%
% description
%    disply help text about Function
%
help(available) ->
   io:fwrite("~w:available(). => [Package1, Package2, ...]~n",
             [?MODULE]),
   io:fwrite("Returns a list of available packages~n"),
   io:fwrite("~w:available(Package). => [Package-vsn1, Package-vsn2, ...]~n",
             [?MODULE]),
   io:fwrite("Returns a list of available versions of package~n");
help(depends) ->
   io:fwrite("~w:depends( Package ). => [Package1, Package2, ...]~n",
             [?MODULE]),
   io:fwrite("Returns a list of all packages that Package depends upon.~n");
help(display) ->
   io:fwrite("~w:display( List_of_strings ). => ok~n",
             [?MODULE]),
   io:fwrite("Will display the result (a list of strings) of a function. ~n"),
   io:fwrite("~w:display( Function ). => ok~n",
             [?MODULE]),
   io:fwrite("Will run a function (without arguments) and the result (a list of strings) is displayed.~n"),
   io:fwrite("Display is a helper for when the result of a function is a too long list.~n");
help(help) ->
   io:fwrite("~w:help(). => ok~n",
             [?MODULE]),
   io:fwrite("Will display a help text about available functions~n"),
   io:fwrite("~w:help( Function ). => ok~n",
             [?MODULE]),
   io:fwrite("Will display a help text about a function~n");
help(install) ->
   io:fwrite("~w:install( Package ). => ok~n",
             [?MODULE]),
   io:fwrite("Will install a package for atmost 60 seconds.~n"),
   io:fwrite("~w:install( Package, Timeout ). => ok~n",
             [?MODULE]),
   io:fwrite("Will install a package for atmost Timeout minutes.~n");
help( installed ) ->
   io:fwrite( "~w:installed(). => [Package1, Package2, ...]~n", [?MODULE] ),
   io:fwrite( "Will return a list of installed packages.~n"),
   io:fwrite( "~w:installed( Package ). => [Package-vsn1, Package-vsn2, ...]~n", [?MODULE] ),
   io:fwrite( "Will return a list of installed versions of Package.~n");
help(installed_dependent_upon) ->
   io:fwrite("~w:installed_dependent_upon( Package ). => [Package1, Package2, ...]~n",
             [?MODULE]),
   io:fwrite("Will return a list of installed packages that depend upon Package.~n");
help(is_installed) ->
   io:fwrite("~w:is_installed( Package ). => true|false~n",
             [?MODULE]),
   io:fwrite("Will return true if a package is installed.~n");
help( otp_version ) ->
       io:fwrite( "~w:otp_version( ). => Version~n", [?MODULE] ),
       io:fwrite( "Will return a version string for OTP~n" );
help(new) ->
   io:fwrite("~w:new( ). => [Package1, Package2, ...]~n",
             [?MODULE]),
   io:fwrite("Will return a list of packages that are new at CEAN archive.~n");
help(newest_available) ->
   io:fwrite("~w:newest_available( Package ). => Package-vsn~n",
             [?MODULE]),
   io:fwrite("Will return the name of newest package with version at CEAN server.~n");
help(newest_installed) ->
   io:fwrite("~w:newest_installed( Package ). => Package-vsn~n",
             [?MODULE]),
   io:fwrite("Will return the newest package version installed.~n");
help( proxy_host ) ->
       io:fwrite( "~w:proxy_host( Host, Port ). => ok~n", [?MODULE] ),
       io:fwrite( "Will make any subsequent requests towards the cean archive use a HTTP proxy. Both arguments should be strings, not atom/integer.~n" ),
       io:fwrite( "~w:proxy_host( \"\", \"\" ) to stop using a proxy.~n", [?MODULE] );
help(proxy_user) ->
   io:fwrite("~w:proxy_user( User, Password ). => ok~n",
             [?MODULE]),
   io:fwrite("Will make any subsequent requests towards the HTTP proxy use user/password. Both arguments should be strings, not atoms.~n");
help(purge) ->
   io:fwrite("~w:purge( ). => ok~n",
             [?MODULE]),
   io:fwrite("Will empty the uninstall directory.~n");
help( server ) ->
       io:fwrite( "~w:server( Server ). => ok~n", [?MODULE] ),
       io:fwrite( "Will make any subsequest requests towards CEAN use Server as the HTTP server. Current server: ~p.~n", [get_server()] );
help(stop) ->
   io:fwrite("~w:stop( ). => ok~n",
             [?MODULE]),
   io:fwrite("Will stop CEAN. Same as q().~n");
help(remote) -> io:fwrite("not implemented.~n");
help(search) ->
   io:fwrite("~w:search( Package ). => ok~n",
             [?MODULE]),
   io:fwrite("Will display a (short) description of package.~n");
help(uninstall) ->
   io:fwrite("~w:uninstall( Package ). => ok~n",
             [?MODULE]),
   io:fwrite("Will uninstall a package by moving it to a uninstall directory.~n");
help(upgrade) ->
   io:fwrite("~w:upgrade( ). => ok~n",
             [?MODULE]),
   io:fwrite("Will upgrade all installed packages for atmost 60 seconds.~n"),
   io:fwrite("~w:upgrade( Timeout ). => ok~n",
             [?MODULE]),
   io:fwrite("Will upgrade all installed packages for atmost Timeout minutes.~n"),
   io:fwrite("~w:upgrade( Package ). => ok~n",
             [?MODULE]),
   io:fwrite("Will _only_ upgrade package, not dependencies.~n"),
   io:fwrite("Will upgrade package for atmost 60 seconds.~n"),
   io:fwrite("~w:upgrade( Package, Timeout ). => ok~n",
             [?MODULE]),
   io:fwrite("Will _only_ upgrade package, not dependencies.~n"),
   io:fwrite("Will upgrade package for atmost Timeout minutes.~n");
help(Function) ->
   case lists:member(Function, exported_functions()) of
     true ->
         io:fwrite("I have forgotten to write help for ~w~n",
                   [Function]);
     false -> io:fwrite("There is no ~w~n", [Function])
   end.

install(Package) when is_atom(Package) ->
   install(erlang:atom_to_list(Package), ?INSTALL_MAX_MINUTES);
install(Package) when is_list(Package) -> install(Package, ?INSTALL_MAX_MINUTES).

install( Package, Timeout ) when is_atom(Package) ->
   install(erlang:atom_to_list(Package), Timeout);
install( Package, Timeout ) when is_list(Package), is_integer(Timeout) ->
%   install(Package, true, false, Timeout).
    try
    [install_package(X, Timeout, package_no_version(X)) || X <- depends(Package)],
    install_package( Package, Timeout, no_vsn(Package) )
    catch
    _:Reason ->
        io:fwrite( "! ~s Error: ~p~n", [Package, Reason]),
        error
    end.


installed() ->
   Contents = lib_dir_visible_contents(),
   lists:usort([no_vsn(LibDir) || LibDir <- Contents]).

installed( Package ) when is_atom(Package) ->
   installed( erlang:atom_to_list(Package) );
installed( Package ) when is_list(Package) ->
       Contents = lib_dir_visible_contents(),
       select_packages( Package, Contents ).

installed_dependent_upon(Package)
   when is_atom(Package) ->
   installed_dependent_upon(atom_to_list(Package));
installed_dependent_upon( Package ) when is_list(Package) ->
       Package_no_version = no_vsn( Package ),
   Fun = fun (Installed) ->
                 case getdeps(Installed) of
                   {ok, Deps} ->
                       lists:member( Package_no_version, Deps );
                   {error, Reason} ->
                       io:fwrite("failed to find dependencies for ~s: ~p~n",
                                 [Installed, Reason]),
                       false
                 end
         end,
   lists:filter(Fun, installed()).

is_installed(Package) when is_atom(Package) ->
   is_installed(atom_to_list(Package));
is_installed( Package ) when is_list(Package) ->
       case no_vsn( Package )
       of Package -> lists:member( Package, installed() )
       ; Package_no_version -> lists:member( Package, installed(Package_no_version) )
       end.

new() ->
   Last = case getlist() of
            {ok, L1} -> lists:sort(L1);
            {error, _} -> []
          end,
   Actual = lib_dir_visible_contents(),
   lists:foldl(fun (Lib, New) ->
                       case lists:member(Lib, Last) of
                         true -> New;
                         false -> [no_vsn(Lib) | New]
                       end
               end,
               [], Actual).

newest_available(Package) ->
   newest_available_from_list(available(Package)).

newest_installed(Package) when is_atom(Package) ->
   newest_installed(erlang:atom_to_list(Package));
newest_installed( Package) when is_list(Package) ->
   newest_available_from_list(installed_package_with_vsn(Package)).

otp_version() ->
   case get_erlang_version() of
     false -> erlang:system_info(otp_release);
     Otp -> Otp
   end.

proxy_host( Host, Port ) when is_list(Host), is_list(Port) ->
   os:putenv(?PROXY_HOST,
             lists:append([Host, ?PROXY_HOST_SEPARATOR, Port])).

proxy_user( User, Password ) when is_list(User), is_list(Password) ->
   os:putenv(?PROXY_USER,
             lists:append([User, ?PROXY_USER_SEPARATOR, Password])).

purge() ->
   Uninstall_dir = uninstall_dir(),
   lists:foreach(fun (Package) ->
                         remove(Uninstall_dir, Package)
                 end,
                 filelib:wildcard("*", Uninstall_dir)).

remote(_Cmd,
      _Args) -> %% TODO
   % connect ssh
   % check machine platform
   % install required bootstrap or lib
   ok.

server( Server ) when is_list(Server) -> os:putenv( "CEAN_SERVER", Server ).

stop() -> init:stop().

search(Word) when is_atom(Word) ->
   search(atom_to_list(Word));
search( Word ) when is_list(Word) ->
   UW = [encode(V) || V <- Word],
   Query = "/packages/index.yaws?action=filter&seek=" ++ UW,
   Url = server_append(Query),
   packages_from_html(Url).

uninstall(Package) when is_atom(Package) ->
   uninstall(atom_to_list(Package));
uninstall( Package) when is_list(Package) ->
   case lists:member( Package, core_packages() ) of
     true ->
         io:fwrite(" can not uninstall ~s. this is a core package.~n",
                   [Package]),
         error;
     false ->
         P = erlang:list_to_atom(Package),
         catch P:stop(),    % just in case
         case installed_dependent_upon(Package) of
           [] ->
               uninstall_many(installed_package_with_vsn(Package)), ok;
           Dependent ->
               io:fwrite("can not uninstall ~s. ~p are dependent upon it.~n",
                         [Package, Dependent]),
               error
         end
   end.

upgrade() -> upgrade( ?UPGRADE_MAX_MINUTES ).

upgrade(Timeout) when is_integer(Timeout) ->
    lists:foreach(fun (Package) -> upgrade(Package, Timeout) end, installed());
upgrade(Package) when is_atom(Package) ->
    upgrade( erlang:atom_to_list(Package), ?UPGRADE_MAX_MINUTES );
upgrade( Package ) when is_list(Package) -> upgrade( Package, ?UPGRADE_MAX_MINUTES ).

upgrade(Package, Timeout) when is_atom(Package) ->
   upgrade(erlang:atom_to_list(Package), Timeout);
upgrade( Package, Timeout ) when is_list(Package), is_integer(Timeout) ->
    case {newest_available(Package), newest_installed(Package)}
    of {[], _Other} -> io:fwrite("~s: no package available~n", [Package])
    ; {Same, Same} -> io:fwrite("~s: no newer package available~n", [Package])
    ; _Else -> install(Package, Timeout)
    end.

%%%%%%%%%%%%%%%%%%%%%% -- internal functions -- %%%%%%%%%%%%%%%%%%%%%%

arch() -> arch(erlang:system_info(system_architecture)).

arch( "i386"++_ ) -> "x86";
arch( "i486"++_ ) -> "x86";
arch( "i586"++_ ) -> "x86";
arch( "i686"++_ ) -> "x86";
arch( "i86pc"++_ ) -> "x86";
arch( Arch ) ->
       case string:chr( Arch, $- )
       of 0 -> Arch
       ; Start -> string:sub_string( Arch, 1, Start - 1 )
       end.

baseurl(Package) ->
       Url = lists:append( [get_repos(), "/", no_vsn(Package)] ),
       server_append(Url).

core_packages() -> ["cean", "ibrowse", "stdlib", "kernel"].

% Put string in file, Consult file, Remove file
consult_string( String ) ->
    File = erlang:ref_to_list(erlang:make_ref()),
    try
    ok = file:write_file( File, String ),
    file:consult( File )
    after
    file:delete( File )
    end.

ebin_dir(Package) ->
   filename:join([code:lib_dir(), Package, "ebin"]).

encode($\s) -> $+;
encode(C) -> C.

exported_functions() ->
   lists:usort([Function
                || {Function, _Arity} <- (?MODULE):module_info(exports),
                   Function =/= module_info]).

% select the packages in Packages that have the same non-version name as Package
select_packages( Package, Packages ) ->
       lists:sort( [X || X <- Packages, no_vsn(X) =:= Package] ).

getarchive(Archive, Timeout) ->
   TarUrl = lists:append([baseurl(Archive), "/", Archive, ".epkg"]),
   case geturl(TarUrl, [{save_response_to_file, true}], Timeout) of
     {ok, _, _, {file, undefined}} ->
         {error, {file, undefined}};
     {ok, _, Header, {file, Archive_contents}} ->
         {Header, Archive_contents};
     {error, Reason} -> {error, Reason}
   end.

% Return list of dependencies for Archive
getdeps( Archive ) -> getdeps_from_proplist( pub(Archive) ).

getdeps_from_proplist( {ok, Proplist} ) ->
    case proplists:get_value( depends, Proplist, not_present )
    of not_present -> {error, package_depends_not_present}
    ; Depends -> {ok, Depends}
    end;
getdeps_from_proplist( Error ) -> Error.

getlist() ->
       case geturl(baseurl("cean") ++ "/available")
       of {ok, "200", _Proplist, List} -> {ok, string:tokens(List, "\n")}
       ; {ok, _Status, _Proplist, Error} -> {error, Error}
       ; {error, Reason} -> {error, Reason}
       end.

geturl(Url) ->
       geturl(Url, [], 1).

geturl(Url, UsrOpts, Timeout) ->
       case get_debug()
       of false -> ok
       ; "" -> ok
       ; _Else -> io:fwrite("GET ~s~n", [Url])
       end,
   ibrowse:start(),
   Opts = proxyopts() ++ UsrOpts,
   Answer = ibrowse:send_req(Url, [], get, [], Opts, Timeout * (?MINUTE)),
   %ibrowse:stop(), replace {stop, shutting_down, ok, State} by {stop, normal, ok, State}
   Answer.

%% Environment interface
%
get_debug() -> os:getenv("CEAN_DEBUG").
get_erlang_version() -> os:getenv("ERLANG_VERSION").
%get_hostarch() -> os:getenv("CEAN_HOSTARCH").
get_repos() ->
       case os:getenv("CEAN_REPOSITORY")
       of false ->
               Name = atom_to_list(element(2, os:type())),
               Arch = arch(),
               OTP = major_otp_version(),
               lists:append(["/bin/", Name, "-", Arch, "/", OTP])
       ; Repos -> Repos
       end.
get_server() ->
       case os:getenv("CEAN_SERVER")
       of false -> ?DEFAULT_SERVER
       ; Server -> Server
       end.


install_package( Package_no_version, Timeout, Package_no_version ) ->
    {ok, Pub} = pub( Package_no_version ),
    Package = package_add_newest_version_from_pub( Package_no_version, Pub ),
    install_package_with_version( Package, Timeout );
install_package( Package, Timeout, _Package_no_version ) ->
    install_package_with_version( Package, Timeout ).


install_package_archive( Package, {error, Reason} ) ->
    erlang:throw( {install_archive_for, Package, Reason} );
install_package_archive( Package, {Header, Archive} ) ->
    %% We have a file (Archive). Delete it before returning.
    %% We can not use 'after' since we are in a 'try'.
    Return = case proplists:get_value( "Content-Type", Header )
% Should be?        of "application/x-gzip" ->
        of "text/plain" -> install_package_binaries( erl_tar:extract(Archive, [memory]) )
        ; Else -> {error, {install_archive_for, Package, header_error, Else}}
        end,
    file:delete( Archive ),
    case Return
    of ok -> io:fwrite("+ ~s~n", [Package]), ok
    ; {error, Reason} -> erlang:throw( Reason )
    end.

install_package_binaries( {ok, Binaries} ) ->
    %% Any errors are throw'n. A single ok is expected.
    [install_package_binary(X) || X <- Binaries],
    ok;
install_package_binaries( {error, Reason} ) -> erlang:throw( {extract_epkg, Reason} ).
install_package_binary( {File, Binary} ) ->
    case zip:extract( Binary, [{cwd, code:lib_dir()}] )
    of {ok, _Files} -> ok
    ; {error, Error} -> erlang:throw( {extract_zip, File, Error} )
    end.


install_package_with_version( Package, Timeout ) ->
    case is_installed( Package )
    of true -> {warning, already_installed}
    ; false ->
        install_package_archive( Package, getarchive(Package, Timeout) )
    end.


installed_package_with_vsn(Package) ->
   filelib:wildcard(Package ++ "-*", code:lib_dir()).


is_newer(Package_that_might_be_newer, Package) ->
   P_newer = [erlang:list_to_integer(N)
              || N <- string:tokens(vsn(Package_that_might_be_newer), ".")],
   P = [erlang:list_to_integer(N)
        || N <- string:tokens(vsn(Package), ".")],
   P_newer > P.

%% @spec lib_dir_visible_contents() -> list()
% input
%
% returns
%    list of the visible contetns of lib dir.
%    unix has hidden contents indicated by a "." first in the name.
%
% execption
%    will exit if code:lib_dir/0 is not a directory
%
lib_dir_visible_contents() ->
   {ok, Contents} = file:list_dir(code:lib_dir()),
   lists:usort([hide_ez_ext(Visible)
                || [First_char | _T] = Visible <- Contents,
                   First_char /= $.]).

hide_ez_ext(Name) ->
   Ext = length(Name) - 3,
   case lists:nthtail(Ext, Name) of
     ".ez" -> lists:sublist(Name, Ext);
     _ -> Name
   end.

major_otp_version() -> major_otp_version( otp_version() ).
major_otp_version( [$R | T] ) -> [$R | lists:takewhile(fun (X) -> (X >= $0) and (X =< $9) end, T)].

newest_available_from_list(Packages) ->
   case lists:sort(fun is_newer/2, Packages) of
     [Newest | _T] -> Newest;
     _Error -> []
   end.

% backwards compatibility
%
no_vsn( Name ) -> package_no_version( Name ).

package_add_newest_version_from_pub( Package, Pub ) ->
    [Newest | _T] = lists:reverse( proplists:get_value(versions, Pub, no_package_versions_found_in_pub) ),
    Package ++ [$- | pub_version_number(Newest)].

packages_from_html(Url) ->
   case geturl(Url) of
     {ok, _, _, Html} -> packages_from_html(Html, []);
     _ -> []
   end.

packages_from_html([], Packages) -> Packages;
packages_from_html(Html, Packages) ->
   case string:str(Html, "class=\"package\"") of
     0 -> Packages;
     Start ->
         Sub = string:sub_string(Html, Start),
         T1 = string:sub_string(Sub, string:chr(Sub, $>) + 1),
         PName = string:sub_string(T1, 1, string:chr(T1, $<) - 1),
         T2 = string:sub_string(T1, string:str(T1, "<i>") + 3),
         Descr = string:sub_string(T2, 1, string:str(T2, "</i>") - 1),
         packages_from_html(T2, [{PName, Descr} | Packages])
   end.

% returns
%       The argument without everything following the last "-"
%       If there is no "-" all of the argument is returned.
%
% execption
%
package_no_version( "ibrowse" ++ _T ) -> "ibrowse"; % ibrowse has - in verison, like this: ibrowse-2.2.0-6
package_no_version( Name ) ->
       case string:rchr( Name, $- )
       of 0 -> Name
       ; Start -> string:sub_string( Name, 1, Start - 1 )
       end.


proxyopts() ->
       Hosts = proxyopts_host( proxyopts(?PROXY_HOST, ?PROXY_HOST_SEPARATOR) ),
       proxyopts_user( proxyopts(?PROXY_USER, ?PROXY_USER_SEPARATOR), Hosts ).

proxyopts(Environment_variable, Separator) ->
   case os:getenv(Environment_variable) of
     false -> [];
     "" -> [];
     Value -> string:tokens(Value, Separator)
   end.

proxyopts_host( [] ) -> [];
proxyopts_host( [Host, Port_string] ) ->
       Port = erlang:list_to_integer(Port_string),
       [{proxy_host, Host}, {proxy_port, Port}].

% Ignore proxy user data when there is no proxy host data.
proxyopts_user( _User, [] ) -> [];
proxyopts_user( [], Hosts ) -> Hosts;
proxyopts_user( [User, Password], Hosts ) -> [{proxy_user, User}, {proxy_password, Password} | Hosts].

%% Return .pub file contents (proplist) for Archive
pub( Archive ) ->
    Url = lists:append([baseurl(Archive), "/", no_vsn(Archive), ".pub"]),
    case geturl(Url)
    of {ok, _, _, "<" ++ _} -> {error, no_pub_file}
    ; {ok, _, _, Pub} -> consult_string( Pub )
    ; Error -> Error
    end.

pub_version_number( {Number, _Date, _} ) -> Number.


remove(Path, File) ->
   Desc = filename:join([Path, File]),
   case filelib:is_dir(Desc) of
     true ->
         case file:list_dir(Desc) of
           {ok, Sub} -> lists:foreach(fun (S) -> remove(Desc, S) end, Sub);
           {error, Reason} -> io:format("error: ~p~n", [Reason])
         end,
         file:del_dir(Desc);
     false -> file:delete(Desc)
   end.

%% @spec server_append(Url::list()) -> list()
% input
%    Url : any string (here used with url)
% returns
%    cean server (as string) appended with url, prepended with "http://".
%
% execption
%
server_append( Url ) -> "http://" ++ get_server() ++ Url.


%% @spec uninstall_many(Packages::list()) -> atom()
% input
%    Packages : list of Application directories
% returns
%
% execption
%    will exit if move of package to Uninstall_dir fails
uninstall_many(Packages) ->
   Lib_dir = code:lib_dir(),
   Uninstall_dir = uninstall_dir(),
   lists:foreach(fun (Package_dir) ->
                         catch remove(Uninstall_dir, Package_dir), % just in case
                         case file:rename(filename:join([Lib_dir, Package_dir]),
                                          filename:join([Uninstall_dir, Package_dir]))
                             of
                           ok ->
                               io:fwrite("- ~s~n", [Package_dir]),
                               code:del_path(ebin_dir(Package_dir));
                           {error, Reason} ->
                               io:fwrite("can not remove ~s: ~s~n",
                                         [Package_dir, Reason])
                         end
                 end,
                 Packages).

%% @spec uninstall_dir() -> list()
% input
%
% returns
%    directory for applications that are uninstalled
%
% execption
%
uninstall_dir() ->
   Uninstall_dir = filename:join([code:root_dir(), "uninstalled"]),
   filelib:ensure_dir(filename:join([Uninstall_dir, "afile"])),
   Uninstall_dir.

vsn(Name) ->
   case lists:dropwhile(fun ($-) -> false;
                            (_) -> true
                        end,
                        Name)
       of
     [_Sep | Vsn] -> Vsn;
     _ -> "0"
   end.

%% @spec call()
% input
%   to be passed as erlang environment arguments
call() ->
   case init:get_plain_arguments() of
     [Node, Cmd | Args] ->
         F = list_to_atom(Cmd),
         N = list_to_atom(Node),
         R = rpc:call(N, ?MODULE, F, Args, 10000),
         io:format("~p~n", [R]);
     Bad -> io:format("{badarg, ~p}~n", [Bad])
   end,
   erlang:halt().

