package("boost_wave_c")
    add_deps("cmake")

    on_install("linux", function (package)
        print("building package")
        import("package.tools.cmake").install(package, configs)
    end)

