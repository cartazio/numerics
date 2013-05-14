import Distribution.Simple
main = defaultMain


{-
copied from 
http://stackoverflow.com/questions/2528887/c-compiler-selection-in-cabal-package 
 on may 10, 2013

also opened a related ticked for cabal 
https://github.com/haskell/cabal/issues/1325#issuecomment-17740585
-}
--main :: IO ()
--main = defaultMainWithHooks simpleUserHooks { buildHook = myBuildHook }


--myBuildHook pkg_descr local_bld_info user_hooks bld_flags =
--    do
--    let lib       = fromJust (library pkg_descr)
--        lib_bi    = libBuildInfo lib
--        custom_bi = customFieldsBI lib_bi
--        cpp_name  = fromJust (lookup "x-cc-name" custom_bi)
--        c_srcs    = (lines . fromJust) (cSources lib_bi)
--        cc_opts   = ccOptions lib_bi
--        inc_dirs  = includeDirs lib_bi
--        lib_dirs  = extraLibDirs lib_bi
--        bld_dir   = buildDir local_bld_info
--    -- Compile C/C++ sources
--    putStrLn "invoking my compile phase"
--    objs <- mapM (compileCxx cpp_name cc_opts inc_dirs bld_dir) c_srcs
--    -- Remove C/C++ source code from the hooked build (don't change libs)
--    let lib_bi'    = lib_bi { cSources = [] }
--        lib'       = lib    { libBuildInfo = lib_bi' }
--        pkg_descr' = pkg_descr { library = Just lib' }
--    -- The following line invokes the standard build behaviour
--    putStrLn "Invoke default build hook"
--    bh <- buildHook simpleUserHooks pkg_descr' local_bld_info user_hooks bld_flags
--    return bh