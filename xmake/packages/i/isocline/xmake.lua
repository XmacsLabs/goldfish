--
-- Copyright (C) 2024 The Goldfish Scheme Authors
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.
--

package("isocline")
    set_homepage("https://github.com/daanx/isocline")
    set_description("Isocline is a portable GNU readline alternative")
    set_license("MIT")

    add_urls("https://github.com/daanx/isocline.git")
    add_versions("v1.0.9", "v1.0.9")

    add_configs("shared", {description = "Build shared library.", default = false, type = "boolean"})

    if is_plat("windows") then
        add_syslinks("user32")  -- Windows 可能需要链接 user32
    elseif is_plat("linux", "bsd") then
        add_syslinks("m", "pthread")  -- Linux/BSD 可能需要数学库和线程库
    end

    on_install(function (package)
        local configs = {}
        if package:config("shared") then
            configs.kind = "shared"  -- 动态库（默认静态库）
        end

        io.writefile("xmake.lua", [[
          add_rules("mode.debug", "mode.release")
          target("libisocline")
              set_kind("static")
              add_files("src/isocline.c")
              add_includedirs("include")
              add_headerfiles("include/*.h")
        ]])
        import("package.tools.xmake").install(package)
    end)

    on_test(function (package)
        assert(package:has_cfuncs("ic_readline", {includes = "isocline.h"}))
    end)
