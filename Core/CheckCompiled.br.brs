20000   library program$ : fncheckcompiled
20020   fncheckcompiled
20900   end 
32000 def library fncheckcompiled
32020   if env$('acsEnableComplier')='Yes' then
32040     dim filename$*256
32060     ! pr 'entered into fncheckcompiled' : pause
32080     execute 'CD S:'
32100     if ~exists('S:\(import)') then execute 'mkdir S:\(import)'
32120     execute 'sy -M '&os_filename$('S:\sortfiles.exe')&' -D . -C ".br.brs|.br"' ioerr DONE
34000     !     open #htmpcmd:=20: 'name=S:\(import)\Tmp'&session$&'.cmd,RecL=256,replace',display,output 
34020     !     pr #htmpcmd: 'prompt $p$g'
34042     !     pr #htmpcmd: '"'&os_filename$('S:\sortfiles.exe')&'" -D . -C ".br.brs|.br"'
34080     !     close #htmpcmd: 
34200     !     execute 'sy "'&os_filename$('S:\(import)\Tmp'&session$&'.cmd')&'"' ! ioerr DONE
34300     !        execute 'free S:\(import)\Tmp'&session$&'.cmd'
36000     ! execute 'sy -M "'&os_filename$('S:\sortfiles.exe')&'" -D . -C ".br.brs|.br"' ioerr DONE
36020     open #h_brsfiles:=20: "Name=S:\(import)\brsfiles",display,input ioerr CC_ERR
36040     linput #h_brsfiles: filename$ eof DONE
36050     ! pr filename$ : pause
36060     if env$('compile_without_asking')='Yes' then 
36080       let docompile=2
36100       let setenv('compile_without_asking','')
36120     else 
36140       let docompile=msgbox("You have uncompiled source files!  Recompile?", "ACS 5 - "&os_filename$(program$), "Yn", "Qst")
36160     end if 
36180     if docompile=2 then 
36200       let setenv("AfterRecompile", "S:\Core\Start")
36220       chain 'S:\Core\ReCompile.br' ! execute "Proc S:\ReCompile.prc" ioerr ignore
36240     end if 
42000     DONE: ! 
42020     close #h_brsfiles: 
42040   end if
42060 fnend 
52000 IGNORE: continue 
62000 CC_ERR: ! r:
62002   let mb_response=msgbox(program$&' encountered an error '&str$(err)&' on line '&str$(line)&'.'&chr$(13)&'Close ACS?'&chr$(13)&'(Choose Cancel for developer pause.)','ACS 5 - S:\Core\CheckCompiled - Error','OKc','Excl')
62004   if mb_response=1 then execute 'system'
62006   pause 
62008   retry  ! /r
