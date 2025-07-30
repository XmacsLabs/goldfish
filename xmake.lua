set_version ("17.11.18")

-- mode
set_allowedmodes("releasedbg", "release", "debug", "profile")
add_rules("mode.releasedbg", "mode.release", "mode.debug", "mode.profile")

-- plat
set_allowedplats("linux", "macosx", "windows", "wasm")

-- proj
set_project("Goldfish Scheme")

-- repo
add_repositories("goldfish-repo xmake")

option("tbox")
    set_description("Use tbox installed via apt")
    set_default(false)
    set_values(false, true)
option_end()

option("repl")
    set_description("Enable REPL (isocline) support")
    set_default(false)
    set_values(false, true)
option_end()

local S7_VERSION = "20250721"
add_requires("s7 "..S7_VERSION, {system=false})

local TBOX_VERSION = "1.7.6"
if has_config("tbox") then
    add_requires("apt::libtbox-dev", {alias="tbox"})
else
    tbox_configs = {hash=true, ["force-utf8"]=true}
    add_requires("tbox " .. TBOX_VERSION, {system=false, configs=tbox_configs})
end

if is_plat("wasm") then
    add_requires("emscripten 3.1.55")
    set_toolchains("emcc@emscripten")
end

local IC_VERSION = "v1.0.9"
add_requires("isocline " .. IC_VERSION, {system=false})

target ("goldfish") do
    set_languages("c++11")
    set_targetdir("$(projectdir)/bin/")
    if is_plat("linux") then
        add_syslinks("stdc++")
    end
    if is_plat("wasm") then
        -- preload goldfish stdlib in `bin/goldfish.data`
        add_ldflags("--preload-file goldfish@/goldfish")
    end
    add_files ("src/goldfish.cpp")
    add_packages("s7")
    add_packages("tbox")

    -- only enable REPL if repl option is enabled
    if has_config("repl") then
        add_packages("isocline")
        add_defines("GOLDFISH_WITH_REPL")
    end

    add_installfiles("$(projectdir)/goldfish/(scheme/*.scm)", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/goldfish/(srfi/*.scm)", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/goldfish/(liii/*.scm)", {prefixdir = "share/goldfish"})
end

target("goldfish_repl_wasm")
    set_kind("binary")
    set_languages("c++11")
    set_targetdir("$(projectdir)/repl/")
    add_files("src/goldfish_repl.cpp")
    add_packages("s7", "tbox")
    if is_plat("wasm") then
        add_defines("GOLDFISH_ENABLE_REPL")
        add_ldflags("--preload-file goldfish@/goldfish")
        -- 导出 REPL 相关函数
        add_ldflags("-sEXPORTED_FUNCTIONS=['_eval_string','_get_out','_get_err','_malloc','_free']", {force = true})
        add_ldflags("-sEXPORTED_RUNTIME_METHODS=['UTF8ToString','allocateUTF8']", {force = true})
        add_ldflags("-sINITIAL_MEMORY=134217728", {force = true})
        add_ldflags("-sALLOW_MEMORY_GROWTH=1", {force = true})
        add_ldflags("-sASSERTIONS=1", {force = true})
        -- 生成 js glue code
        set_extension(".js")
    end

includes("@builtin/xpack")

xpack ("goldfish")
    set_formats("deb", "rpm", "srpm")
    set_author("Da Shen <da@liii.pro>")
    set_license("Apache-2.0")
    set_title("Goldfish Scheme")
    set_description("A Python-like Scheme Interpreter") 
    set_homepage("https://gitee.com/LiiiLabs/goldfish")
    add_targets ("goldfish")
    add_sourcefiles("(xmake/**)")
    add_sourcefiles("xmake.lua")
    add_sourcefiles("(src/**)")
    add_sourcefiles("(goldfish/**)")
    on_load(function (package)
        if package:with_source() then
            package:set("basename", "goldfish-$(plat)-src-v$(version)")
        else
            package:set("basename", "goldfish-$(plat)-$(arch)-v$(version)")
        end
    end)

