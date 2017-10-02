01000 fn_setup
14080 fntop(program$)
14100 fnHamsterFio('CO Favorites')
14120 xit: fnxit
24000 def fn_setup
24020   if ~setup then
24040     setup=1
24060     library 'S:\Core\Library': fntop,fnxit,fnHamsterFio,fnOpenFile,fnCloseFile,fnerror,fnAddOneC,fnBuildKey$
24070     on error goto ERTN
24080     dim form$(0)*256
24100     dim favData$(0)*128,favDataN(0)
24140   end if
24160 fnend
34000 def library fnFavoriteAdd(programCaption$*256)
34020   if ~setup then let fn_setup
34040   hFav=fn_open('CO Favorites',mat favData$,mat favDataN,mat form$)
34060   mat favData$=('') : mat favDataN=(0)
34080   favData$(fav_user)=env$('Unique_Computer_ID')
34100   favData$(fav_system)=env$('cursys')
34110   favData$(fav_program)=programCaption$
34120   write #hFav,using form$(hFav): mat favData$,mat favDataN
34140   fnCloseFile(hFav,'CO Favorites')
34160 fnend
36000 def library fnFavoriteDel(programCaption$*256)
36020   if ~setup then let fn_setup
36040   hFavProgram=fn_open('CO Favorites',mat favData$,mat favDataN,mat form$,0,2)
36060   mat favData$=('') : mat favDataN=(0)
36080   favData$(fav_user)=env$('Unique_Computer_ID')
36100   favData$(fav_system)=env$('cursys')
36120   favData$(fav_program)=programCaption$
36140   delete #hFavProgram,key=fnBuildKey$('CO Favorites',mat favData$,mat favDataN, 2):
36160   fnCloseFile(hfav,'CO Favorites')
36180 fnend
38000 def library fnFavoriteList(mat favorite$)
38010   if ~setup then let fn_setup
38012   mat favorite$(0)
38020   hFav=fn_open('CO Favorites',mat favData$,mat favDataN,mat form$,1)
38040   restore #hFav,key>=fn_favKey$: nokey fl_eof
38060   do
38080     read #hFav,using form$(hFav):mat favData$,mat favDataN eof fl_eof
38100     flMatch=0
38120     if rtrm$(favData$(fav_user))=env$('Unique_Computer_ID') and favData$(fav_system)=env$('cursys') then
38140       flMatch=1
38150       favoriteListReturn+=1
38160       fnAddOneC(mat favorite$,favData$(fav_program))
38180     end if
38200   loop while flMatch
38220   fl_eof: !
38240   fnCloseFile(hFav,'CO Favorites')
38250   fnFavoriteList=favoriteListReturn
38260 fnend
42000 def fn_favKey$*42
42020   dim favKeyReturn$*42
42040   if favKeySetup$<>env$('cursys') then
42060     favKeySetup$=env$('cursys')
42080     mat favData$=('')
42100     mat favDataN=(0)
42120     favData$(fav_user)=env$('Unique_Computer_Id')
42140     favData$(fav_system)=env$('cursys')
42160     favKeyReturn$=fnBuildKey$('CO Favorites',mat favData$,mat favDataN, 1)
42180   end if
42200   fn_favKey$=favKeyReturn$
42220 fnend
76000 ! <updateable region: fn_open (supressprompt:=2)>  
76020 def fn_open(filename$*255, mat f$, mat fn, mat form$; inputonly, keynum, dont_sort_subs, path$*255, mat descr$, mat field_widths,dontupdate,___,index)
76040   dim _fileiosubs$(1)*800, loadedsubs$(1)*32
76060   let fn_open=fnOpenFile(filename$, mat f$, mat fn, mat form$, inputonly, keynum, dont_sort_subs, path$, mat descr$, mat field_widths, mat _fileiosubs$,supressprompt:=2)
76080   if ~max(srch(loadedsubs$,uprc$(filename$)),0) then 
76100     mat loadedsubs$(udim(loadedsubs$)+1) 
76120     let loadedsubs$(udim(loadedsubs$))=uprc$(filename$)
76140     for index=1 to udim(mat _fileiosubs$) 
76160       execute (_fileiosubs$(index)) 
76180     next index
76200   end if
76220 fnend
76240 ! </updateable region: fnopen>
76260 ! <updateable region: ertn>
76280 ERTN: let fnerror(program$,err,line,act$,"xit")
76300   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
76320   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT 
76340   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
76360 ERTN_EXEC_ACT: execute act$ : goto ERTN
76380 ! </updateable region: ertn>
