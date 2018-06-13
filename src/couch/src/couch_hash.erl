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

-export([hash/1, hash_final/1, hash_init/0, hash_update/2]).

-on_load(init/0).

init() ->
    NumScheds = erlang:system_info(schedulers),
    Dir = code:priv_dir(couch),
    ok = erlang:load_nif(filename:join(Dir, ?MODULE), NumScheds).

hash(Data) ->
    case info_fips_nif() of
        "enabled" ->
            erlang:md5(Data);
        "not_enabled" ->
            crypto:hash(md5, Data);
        "not_supported" ->
            crypto:hash(md5, Data)
    end.

hash_final(Context) ->
    case info_fips_nif() of
        "enabled" ->
            erlang:md5_final(Context);
        "not_enabled" ->
            crypto:hash_final(Context);
        "not_supported" ->
            crypto:hash_final(Context)
    end.

hash_init() ->
    case info_fips_nif() of
        "enabled" ->
            erlang:md5_init();
        "not_enabled" ->
            crypto:hash_init(md5);
        "not_supported" ->
            crypto:hash_init(md5)
    end.

hash_update(Context, Data) ->
    case info_fips_nif() of
        "enabled" ->
            erlang:md5_update(Context, Data);
        "not_enabled" ->
            crypto:hash_update(Context, Data);
        "not_supported" ->
            crypto:hash_update(Context, Data)
    end.

info_fips_nif() ->
    "not_supported".
