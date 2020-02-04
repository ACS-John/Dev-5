if ~exists('S:\(import)') then execute 'mkdir S:\(import)'
execute 'sy dir '&os_filename$('S:\')&'\*.br.brs /s /b >'&os_filename$('S:\(import)\brsfiles')
setenv('compile_without_asking','')
setenv("AfterRecompile", "S:\Core\Start")
library 'S:\Core\ReCompile.br': fnReCompile
fnReCompile(1)
! chain 'S:\Core\ReCompile.br' ! execute "Proc S:\ReCompile.prc" ioerr ignore
end
