/**
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

#ifdef __WIN32__
    #include <windows.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <erl_nif.h>

#define OPENSSL_THREAD_DEFINES
#include <openssl/opensslconf.h>
#include <openssl/crypto.h>
#include <openssl/objects.h>
#include <openssl/err.h>

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

/* The NIFs: */
static ERL_NIF_TERM info_fips(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ErlNifFunc nif_funcs[] = {
    {"info_fips_nif", 0, info_fips}
};

ERL_NIF_INIT(couch_hash,nif_funcs,load,NULL,upgrade,unload)

#ifdef FIPS_SUPPORT
static ERL_NIF_TERM atom_enabled;
static ERL_NIF_TERM atom_not_enabled;
#else
static ERL_NIF_TERM atom_not_supported;
#endif

static int library_refc = 0; /* number of users of this dynamic library */
static int library_initialized = 0;

#define PRINTF_ERR0(FMT)
#define PRINTF_ERR1(FMT,A1)
#define PRINTF_ERR2(FMT,A1,A2)

static int verify_lib_version(void)
{
    const unsigned long libv = SSLeay();
    const unsigned long hdrv = OPENSSL_VERSION_NUMBER;

#   define MAJOR_VER(V) ((unsigned long)(V) >> (7*4))

    if (MAJOR_VER(libv) != MAJOR_VER(hdrv)) {
	PRINTF_ERR2("CRYPTO: INCOMPATIBLE SSL VERSION"
		    " lib=%lx header=%lx\n", libv, hdrv);
	return 0;
    }
    return 1;
}

static int initialize(ErlNifEnv* env, ERL_NIF_TERM load_info)
{

    int tpl_arity;
    const ERL_NIF_TERM* tpl_array;
    int vernum;
    ErlNifBinary lib_bin;

    if (!verify_lib_version())
	return __LINE__;

    /* load_info: {302, <<"/full/path/of/this/library">>,true|false} */
    if (!enif_get_tuple(env, load_info, &tpl_arity, &tpl_array)
	|| tpl_arity != 3
	|| !enif_get_int(env, tpl_array[0], &vernum)
	|| vernum != 302
	|| !enif_inspect_binary(env, tpl_array[1], &lib_bin)) {

	PRINTF_ERR1("CRYPTO: Invalid load_info '%T'", load_info);
	return __LINE__;
    }

    if (library_initialized) {
	/* Repeated loading of this library (module upgrade).
	 * Atoms and callbacks are already set, we are done.
	 */
	return 0;
    }

#ifdef FIPS_SUPPORT
    atom_enabled = enif_make_atom(env,"enabled");
    atom_not_enabled = enif_make_atom(env,"not_enabled");
#else
    atom_not_supported = enif_make_atom(env,"not_supported");
#endif

    library_initialized = 1;
    return 0;
}

static ERL_NIF_TERM info_fips(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef FIPS_SUPPORT
    return FIPS_mode() ? atom_enabled : atom_not_enabled;
#else
    return atom_not_supported;
#endif
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    int errline = initialize(env, load_info);
    if (errline) {
	return errline;
    }

    *priv_data = NULL;
    library_refc++;
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
    int errline;
    if (*old_priv_data != NULL) {
	return __LINE__; /* Don't know how to do that */
    }
    if (*priv_data != NULL) {
	return __LINE__; /* Don't know how to do that */
    }
    errline = initialize(env, load_info);
    if (errline) {
	return errline;
    }
    library_refc++;
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
    --library_refc;
}
