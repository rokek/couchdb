% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_hash).

-export([hash/1]).

-on_load(init/0).

init() ->
    NumScheds = erlang:system_info(schedulers),
    Dir = code:priv_dir(couch),
    ok = erlang:load_nif(filename:join(Dir, ?MODULE), NumScheds).

hash(Data) ->
    case info_fips_nif() of
        "enabled" ->
            hexstring(erlang:md5(Data));
        "not_enabled" ->
            crypto:hash(md5, Data);
        "not_supported" ->
            crypto:hash(md5, Data)
    end.

hexstring(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X])).

info_fips_nif() ->
    "not_supported".
