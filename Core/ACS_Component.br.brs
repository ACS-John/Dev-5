08020 def fn_setup
08040   if ~setup_library then 
08060     setup_library=1
08080     library 'S:\Core\Library': fnprg,fnerror
08100     library 'S:\Core\Library': fnmsgbox,fnpause
08120     library 'S:\Core\Library': fngethandle,fncmdkey
08140     library 'S:\Core\Library': fndate_picker$
08150     library 'S:\Core\Library': fnlistprint
08160     library 'S:\Core\Library': fnCopy
08170   end if 
08180   on error goto ERTN
08200   dim _program$(1)*255
08220   dim _menu$(1)*255
08222   dim combooptionsetlist$(0)*256
08240   tab$=hex$('09')
08260   setup=1
08280 fnend 
09000 def library fntop(;prg$*256,cap$*128)
09020   !  top of (every) program function
09040   if ~setup then let fn_setup
09060   pr newpage
09080   cap$=trim$(cap$)
09100   prg$=trim$(prg$)
09120   if cap$='' and prg$<>'' then 
09140     cap$=prg$(pos(prg$,'\',-1)+1:pos(prg$,'.',-1)-1)
09160   else if cap$='' then 
09180     cap$="(untitled)"
09200   end if 
09220   if prg$='' then 
09240     prg$="(unknown)"
09260     if env$('acsDeveloper')<>'' then pr 'fnTop called but program was not set.' : pause 
09280   end if 
09300   fnprg(prg$,put=2)
09320   setenv('Program_Caption',cap$)
09330   if env$('acsDeveloper')='' then
09340     if ~exists('FileIO.ini') then
09360       fnCopy('S:\FileIO.ini','FileIO.ini')
09380     end if
09400     if ~exists('ScreenIO.ini') then
09420         fnCopy('S:\ScreenIO.ini','ScreenIO.ini')
09440     end if
09450   end if
09460 fnend 
10000 def library fntos(sn$*100)
10020   ! screen ace top of screen function
10040   if ~setup then let fn_setup
10060   sn$=trim$(sn$&session$)
10080   tmp_combo_count_for_set=0
10100   combooptionsetlistcount=combooption_which_prior=combooption_which=0
10120   combokeycurrent$=combokeyprior$=''
10140   if len(sn$)>100 then : pr "INVALID FILE NAME: Too Long" : input fields "1,1,C 1,N": pause$ : goto XIT
10160   ! close #119: ioerr ignore
10180   ! open #119: "Name="&env$('temp')&'\'&sn$&",RecL=1024,Replace",internal,outin,relative  ! recl was 500
10200   fn_clear_env
10220   if env$('GUIMode')='OFF' then execute 'config GUI On'
10240 fnend 
11000 def library fnlbl(myline,mypos,txt$*200; mylen,myalign,font_mod,container,tabcon,lbl_tooltip$*256)
11020   ! add a label to a screen ace form   ! fnlbl(l,p,t$*200; mylen,myalign,fm,c,tc)
11040   ! myline   vertical (that's up and down) Position of the Label.
11060   ! mypos    horizontal (left and right) position of the label.
11080   ! txt$     visible text/caption of the label.
11100   ! mylen    maximum length of the label (measured in characters)
11120   !          if unspecified, it defaults to len(txt$)
11140   ! myalign  alignment of the text within the label
11160   !          0=left,  1=right,  2=center
11180   if ~setup then let fn_setup
11200   if mylen=0 then mylen=len(txt$)
11220   setenv('control'&str$(fn_control_count), "LABEL|"&str$(myline)&"|"&str$(mypos)&"|"&str$(mylen)&"|"&str$(myalign)&"|"&txt$&"|"&str$(container)&"|"&str$(tabcon)&"|"&str$(font_mod)&"|"&lbl_tooltip$)
11240 fnend 
12000 def library fnpic(lyne,ps,height,width,picture$*300;con,tabcon)
12020   ! add a picture/image to a screen ace form
12040   if ~setup then let fn_setup
12060   setenv('control'&str$(fn_control_count), "PICTURE|"&str$(lyne)&"|"&str$(ps)&"|"&str$(width)&"|"&str$(height)&"|"&picture$&"|")
12080 fnend 
12160 def library fntxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
12162   if ~setup then let fn_setup
12180   ! screen ace text box
12200   if lwrc$(mask$)='mmddyy' then 
12220     mask$='1'
12240   else if lwrc$(mask$)='ccyymmdd' then 
12260     mask$='3'
12280   else if lwrc$(mask$)='pointtwo' then 
12300     mask$='32'
12320   else if lwrc$(mask$)='number' then 
12340     mask$='30'
12360   else if lwrc$(mask$)='currency' then 
12380     mask$='10'
12400   end if 
12420   if mask$='1' then 
12440     width=8
12460   else if mask$='2' then 
12480     width=10
12500   else if mask$='3' then 
12520     width=10
12540   else if mask$='4' then 
12560     width=10
12580   else if mask$='5' then 
12600     width=8
12620   end if 
12640   if val(mask$)>=10 and val(mask$)<=49 then ali=1
12660   if maxlen=0 then maxlen=width
12680   setenv('control'&str$(fn_control_count), "TEXT|"&str$(lyne)&"|"&str$(ps)&"|"&str$(width)&"|"&str$(maxlen)&"|"&str$(ali)&"|"&str$(disable)&"|"&mask$&"|"&tooltip$&"|"&str$(contain)&"|"&str$(tabcon)&"|"&addtomask$&"|")
12700 fnend 
12720 def library fnmultiline(lyne,ps,height,width;contain,tabcon,tooltip$*200)
12740   ! add a multiline text box to a screen ace form
12760   if ~setup then let fn_setup
12780   setenv('control'&str$(fn_control_count), "MULTILINE|"&str$(lyne)&"|"&str$(ps)&"|"&str$(height)&"|"&str$(width)&"|"&tooltip$&"|"&str$(contain)&"|"&str$(tabcon)&"|")
12800   lyne=ps=height=width=contain=tabcon=0
12820   tooltip$=""
12840 fnend 
12860 def library fnopt(lyne,ps, txt$*196; align,contain,tabcon)
12880   ! lyne      vertical (that's up and down) Position of the Option.
12900   ! ps        horizontal (left and right) position of the option.
12920   ! txt$      visible text/caption
12940   ! align     0=left,  1=right
12960   ! contain   container number (for containers like frames and tab strips)
12980   if ~setup then let fn_setup
13000   if align=1 then ps=ps-len(trim$(txt$))
13020   setenv('control'&str$(fn_control_count), "OPTION|"&str$(lyne)&"|"&str$(ps)&"|"&str$(align)&"|"&txt$&"|"&str$(contain)&"|"&str$(tabcon)&"|")
13040 fnend 
13060 def library fnchk(lyne,ps,txt$*196; align,contain,tabcon,chk_disable)
13080   ! add a screen ace check box
13100   if ~setup then let fn_setup
13120   if align=1 then ps=ps-len(trim$(txt$))-2
13140   setenv('control'&str$(fn_control_count), "CHECK|"&str$(lyne)&"|"&str$(ps)&"|"&str$(align)&"|"&txt$&"|"&str$(contain)&"|"&str$(tabcon)&"|"&str$(chk_disable)&"|")
13160 fnend 
13180 def library fncomboa(sfn$*256,lyne,ps,mat opt$;ttt$*200,width,contain,tabcon)
13182   if ~setup then let fn_setup
13184   fncomboa=fn_comboa(sfn$,lyne,ps,mat opt$, ttt$,width,contain,tabcon)
13185 fnend 
13186 def fn_comboa(sfn$*256,lyne,ps,mat opt$;ttt$*200,width,contain,tabcon,comboa_combooptionset$*256)
13188   if env$('exitnow')='yes' then goto COMBOA_COMPLETE ! special processing to increase speed for exitnow
13200   ! add a combo box (populated from an array) to a screen ace form
13220   ! sfn$      simple file name
13240   !           (used to store options in to be passed to win6)
13260   ! mat opt$  choices in the combobox and one of them is your answer
13280   ! width     (optional) sets the field/max length of the combobox.
13300   !           may not be larger than 81
13360   ! __________________________________________________
13380   ! ** get/set constants ********
13400   ! fncno(cno)
13420   sfn$=trim$(sfn$)&env$('cno')
13440   if width=0 then 
13460     for j=1 to udim(mat opt$)
13480       width=max(width,len(opt$(j)))
13500     next j
13520   end if 
13962   fn_add_combo_option_list('','',1)
13980   for rec_count=1 to udim(mat opt$)
14010     fn_add_combo_option_list(opt$(rec_count)(1:81),opt$(rec_count)(1:81))
14020   next rec_count
14060   setenv('control'&str$(fn_control_count),"COMBOA|"&str$(lyne)&"|"&str$(ps)&"|"&str$(width)&"|0|"&sfn$&"[SESSION].tmp|1|"&ttt$&"|"&str$(contain)&"|"&str$(tabcon)&"|"&comboa_combooptionset$&"|")
14070   fn_combooptionsetlist_add(comboa_combooptionset$)
14080   width=contain=0
14100   COMBOA_COMPLETE: ! 
14120 fnend 
14140 def library fncombof(sfn$*100,lyne,ps,width,df$*200,psk,lnk,psd,lnd; if$*200,limlis,urep,ttt$*200,contain,tabcon)
14142   if env$('exitnow')='yes' then goto COMBOF_COMPLETE ! special processing to increase speed for exitnow
14160   ! add a combo box (populated from a file) to a screen ace form
14180   if ~setup then let fn_setup
14200   dim key$*30,desc$*120,form$*200,becky$*199
14220   dim ml$(10)*256
14240   ! __________________________________________________
14260   ! df$ (data file) must be internal br format
14280   ! psk (key position)=your key's/answer's starting pos
14300   ! lnk (key length)
14320   ! psd (description position)
14340   ! lnd (description length)
14360   ! if$ (index file)
14380   ! limlis (limit to list)=0=no,  1=yes,  2=yes, but add a [all] option
14400   ! urep  (use or replace)=0=make new/replace,  1=use if possible
14420   ! __________________________________________________
14440   ! **** get/set constants **********
14500   lnk=min(lnk,30) : lnd=min(lnd,60)
14520   width=min(width,81) : sfn$=trim$(sfn$) : df$=trim$(df$)
14540   if$=trim$(if$)
14560   form$="Form Pos "&str$(psk)&",C "&str$(lnk) : nodesc=1
14580   if psd<>0 and lnd<>0 then 
14600     form$=form$&",Pos "&str$(psd)&",C "&str$(lnd) : nodesc=0
14620   end if 
14640   becky$=sfn$&env$('cno')&"[SESSION].tmp" ! combof_whr$=env$('temp')&'\'&becky$
14660   ! __________________________________________________
14680   if width=0 then width=lnk+lnd+1
14700   dim combokeycurrent$*512,combokeyprior$*512
14710   combokeycurrent$='df='&df$&',if='&if$&',psk='&str$(psk)&',lnk='&str$(lnk)&',psd='&str$(psd)&'lnd='&str$(lnd)&',limlis='&str$(limlis)
14720   !     if combokeycurrent$=combokeyprior$ and combokeyprior$<>'' then
14730   !       acol_env_variable$='tmp_combo'&str$(tmp_combo_count_for_set+=1)
14740   !       setenv(acol_env_variable$&'_key','dupe') ! XXX env$(acol_env_variable_prior$&'_key')) ! pr 'adding first key: '&key$ : pause
14750   !       setenv(acol_env_variable$,'dupe') ! XXX env$(acol_env_variable_prior$))
14752   ! !     pr 'saved time here ';savedtimecount+=1
14760   !       goto EODF
14770   !     else 
15000   if if$="" then 
15020     open #df:=fngethandle: "Name="&df$&",Shr",internal,input 
15040   else 
15060     open #df:=fngethandle: "Name="&df$&",KFName="&if$&",Shr",internal,input,keyed ioerr COMBOF_OPEN_IOERR
15080   end if 
15122   fn_add_combo_option_list('','',1)
15160   if limlis=2 then 
15190     fn_add_combo_option_list("[All]","[All]")
15200   end if 
15220   do  ! READ_DF: !
15240     if nodesc=0 then 
15260       read #df,using form$: key$,desc$ eof EODF ioerr ERR_READ
15290       fn_add_combo_option_list(rpad$(trim$(key$),lnk),rpad$(trim$(key$),lnk)&" "&desc$)
15300     else 
15320       desc$=''
15340       read #df,using form$: key$ eof EODF ioerr ERR_READ
15370       fn_add_combo_option_list(rpad$(trim$(key$),lnk),rpad$(trim$(key$),lnk))
15380     end if 
15420   loop  ! goto READ_DF
15430   !     end if
15440   ! __________________________________________________
15460   ERR_READ: ! r:
15480   ! this whole routine is untested because I've yet to see a need for it - but if there is a need for it - it should work...
15500   ! just comment in the ioerrs on the reads in read_df above
15520   if err=61 then 
15540     mat ml$(2)
15560     ml$(1)='Combo Box creation has encountered a locked record'
15580     ml$(2)='The locked record ('&str$(rec(df))&'will be skipped.'
15600     fnmsgbox(mat ml$, resp$, 'ComboBox Record Lock Error',0)
15620   else 
15640     goto ERTN
15660   end if 
15680   read #df,release: 
15700   continue  ! /r
15710   ! __________________________________________________
15720   EODF: ! 
15722   acol_env_variable_prior$=acol_env_variable$
15724   combokeyprior$=combokeycurrent$
15730   close #df: ioerr ignore
15750   goto WRITE_IT
15760   COMBOF_OPEN_IOERR: ! r:
15770   pr '!! COMBOF_OPEN_ERROR !!'
15780   pr '  Name= '
15790   pr '    '&df$
15800   pr '  KFName= '
15810   pr '    '&if$
15820   pr '  err='&str$(err)&' on line '&str$(line)
15830   ! 
15840   fnpause
15850   goto WRITE_IT ! /r
15860   WRITE_IT: ! 
15870   setenv('control'&str$(fn_control_count),"COMBOF|"&str$(lyne)&"|"&str$(ps)&"|"&str$(width)&"|"&str$(lnk)&"|"&becky$&"|"&str$(limlis)&"|"&ttt$&"|"&str$(contain)&"|"&str$(tabcon)&"|"&combokeycurrent$&"|")
15872   fn_combooptionsetlist_add(combokeycurrent$)
15880   COMBOF_COMPLETE: ! 
15890 fnend 
15892 def fn_combooptionsetlist_add(cosladd$*256)
15894   mat combooptionsetlist$(combooptionsetlistcount+=1)
15895   !   mat comboOptionItemList(comboOptionSetListCount)
15896   combooptionsetlist$(combooptionsetlistcount)=cosladd$
15897   !   comboOptionItemList(comboOptionSetListCount)=tmp_combo_count_for_set ! control_count
15898 fnend 
15900 def fn_add_combo_option_list(key$*81,txt$*81; reset_only)
15902   key$=trim$(key$)
15904   txt$=trim$(txt$)
15910   if reset_only then 
15912     acol_env_variable$='tmp_combo'&str$(tmp_combo_count_for_set+=1) ! control_count is not right - it's a response count
15915     setenv(acol_env_variable$,'') ! PR 'setting up '&acol_env_variable$
15920     acol_is_first=1
15925   else if acol_is_first then 
15930     acol_is_first=0
15935     setenv(acol_env_variable$&'_key',key$) ! pr 'adding first key: '&key$ : pause
15940     setenv(acol_env_variable$,txt$)
15945   else 
15946     setenv(acol_env_variable$&'_key',env$(acol_env_variable$&'_key')&'|'&key$)
15950     setenv(acol_env_variable$,env$(acol_env_variable$)&'|'&txt$)
15955   end if 
15960 fnend 
16000 def library fnflexinit1(sfn$*100,lyne,ps,height,width,mat ch$;mat cm$,seltype,usr,con,tabcon)
16002   ! mat ch$   (column headers)=no more than 80 headers with 100 chrs each
16004   ! mat cm$   (column mask)=(see mask chart in screen ace manual)
16006   ! seltype   0=editable cells,  1=row selection,  2=column selection
16008   ! usr       (use or repl)=0=replace/build new,
16010   !                        >0=use previous (sorta disables fnflexadd1
16012   !                        =-1=append previous
16020   ! sfn$      (simple file name) specific file you want flexgrid stored
16040   !           do not use an extension on the file name
16060   if ~setup then let fn_setup
16062   if env$('exitnow')='yes' then goto FLEXINIT1_COMPLETE ! special processing to increase speed for exitnow
16080   dim hdrfile$*192,all_hdr$*6491,all_mask$*6491
16081   dim optfile$*199
16082   if usr=0 then grid_populated=0
16100   ! __________________________________________________
16120   ! if usr=0 then pr "USR=0-Replace"
16140   ! if usr>0 then pr "USR>0-Use Previous USR="&str$(usr)
16160   ! if usr<0 then pr "USR<0-append=-1    USR="&str$(usr)
16180   sfn$=trim$(sfn$)
16200   if sfn$='' then 
16220     pr 'SFN$ is required for Flex grids'
16260     pr 'Press Enter to continue without the flex grid'
16280     pause 
16290     goto FLEXINIT1_COMPLETE
16300   end if 
16320   if ~setup then let fn_setup
16340   ! fncno(cno)
16360   all_hdr$=all_mask$=""
16380   ! fn_get_flexhandle
16400   filenumber=fn_get_flexhandle(1)
16420   sfn$=trim$(sfn$)&env$('cno') : optfile$=sfn$&"[SESSION].tmp"
16440   hdr_count=udim(ch$) : hdrfile$=sfn$&".hdr"
16460   if usr<>0 then goto USEPREVIOUS
16480   XRETRY: ! !print "Retrying delete here! (If you see this twice)"
16500   if exists(env$('temp')&'\'&optfile$) then execute '*free "'&env$('temp')&'\'&optfile$&'" -n' ioerr ignore ! retry
16520   close #filenumber: ioerr ignore
16540   if exists(env$('temp')&'\'&hdrfile$)<>0 then 
16560     execute '*free "'&env$('temp')&'\'&hdrfile$&'" -n' ioerr XRETRY
16580   end if 
16600   USEPREVIOUS: ! 
16620   if usr>0 and exists(env$('temp')&'\'&optfile$) then 
16640     fnflexinit1=1 : goto WRITE_TO_ACE
16660   end if 
16680   ! _______________________
16700   ! ***  test validity of some stuff **********
16720   fnflexinit1=555
16740   open #filenumber: "Name="&env$('temp')&'\'&hdrfile$&",Size=0,Replace,EoL=CRLF,RecL=8000",display,output 
16780   for j=1 to udim(cm$)
16800     if trim$(cm$(j))="" then cm$(j)="80"
16820   next j
16840   for j=1 to udim(ch$) : all_hdr$=all_hdr$&ch$(j)&chr$(9) : next j
16860   for j=1 to udim(cm$) : all_mask$=all_mask$&cm$(j)&chr$(9) : next j
16880   pr #filenumber,using "Form pos 1,C "&str$(len(all_hdr$)): all_hdr$
16900   pr #filenumber,using "Form pos 1,C "&str$(len(all_mask$)): all_mask$
16920   close #filenumber: 
16940   ! hdrfile name is expected by screen ace to be the same name as
16960   ! .  ! optfile$ only with the added .hdr extenstion
16980   if usr>0 and exists(env$('temp')&'\'&optfile$)<>0 then 
17000     fnflexinit1=1 : goto WRITE_TO_ACE
17020   end if 
17040   ! __________________________________________________
17060   fnflexinit1=0
17080   if exists(env$('temp')&'\'&optfile$)<>0 then 
17100     execute "*free "&env$('temp')&'\'&optfile$&" -n" ioerr ignore
17120   end if 
17140   open #filenumber: "Name="&env$('temp')&'\'&optfile$&",Size=0,Replace,EoL=CRLF,RecL=6491",display,output 
17160   WRITE_TO_ACE: ! 
17180   sorttype=0
17200   setenv('control'&str$(fn_control_count),"FLEX|"&str$(lyne)&"|"&str$(ps)&"|"&str$(height)&"|"&str$(width)&"|2|"&str$(seltype)&"|"&str$(sorttype)&"|"&sfn$&"|"&str$(hdr_count)&"|"&str$(con)&"|"&str$(tabcon)&"|")
17220   usr=0
17240   FLEXINIT1_COMPLETE: ! 
17260 fnend 
17280 def library fnflexadd1(mat item$) ! this function may need to be updated to save data in a work file for re-adding later; this is due to error 980 when closing a list with all records filtered; Gordon should fix -- 5/12/14
17281   ! add a line to a flexgrid on a screen ace form
17282   if env$('exitnow')='yes' then goto FLEXADD1_COMPLETE ! special processing to increase speed for exitnow
17320   if ~setup then let fn_setup
17340   dim all_item$*6491
17360   mat2str(mat item$,all_item$,hex$('09'))
17380   flexhandle=fn_get_flexhandle
17382   grid_populated+=1
17400   pr #flexhandle,using "Form pos 1,C "&str$(len(all_item$)): all_item$ ioerr ignore
17410   FLEXADD1_COMPLETE: ! 
17420 fnend 
17440 def library fnfra(lyne,ps,hi,wd; cap$*128,tooltip$*300,contain,tabcon)
17460   ! add a frame to a screen ace form
17480   if ~setup then let fn_setup
17500   setenv('control'&str$(fn_control_count),"FRAME|"&str$(lyne)&"|"&str$(ps)&"|"&str$(hi)&"|"&str$(wd)&"|"&cap$&"|"&tooltip$&"|"&str$(contain)&"|"&str$(tabcon)&"|")
17520 fnend 
17540 def library fntab(myline,mypos,height,width,mat cap$)
17560   ! myline sets the vertical (up and down) position
17580   ! mypos sets the horizontal (left and right) position
17600   ! height/width  -  duh
17620   ! tabsperline=(tpl)=how many tabs will fit on each row of tabstrip
17640   ! mat cap$=the captions on the tabs you want.
17660   ! udim(mat cap$) will set the number of tabs
17680   ! each tab caption should not be longer than 80 characters
17700   ! no more than 99 tabs
17720   if ~setup then let fn_setup
17740   open #tf1:=fngethandle: "Name="&env$('temp')&"\tab.txt,size=0,RecL=80,replace",internal,output 
17760   for j=1 to udim(mat cap$)
17780     write #tf1,using "Form Pos 1,C 80": cap$(j)(1:80)
17800   next j
17820   close #tf1: 
17840   setenv('control'&str$(fn_control_count),"TAB|"&str$(myline)&"|"&str$(mypos)&"|"&str$(height)&"|"&str$(width)&"|"&env$('temp')&"\|tab.txt|")
17860 fnend 
17880 def library fncmdkey(caption$*200,returnkey; default,cancel,tt$*200)
17900   ! add a button to a screen ace form
17920   if ~setup then let fn_setup
17940   setenv('control'&str$(fn_control_count),"CMDKEY|"&caption$&"|"&str$(returnkey)&"|"&str$(default)&"|"&str$(cancel)&"|"&tt$&"|")
17960 fnend 
17980 def library fncmdset(bon)
18000   if ~setup then let fn_setup
18020   if bon=1 then 
18040     fncmdkey("&Cancel",5,1,1)
18060   else if bon=2 then 
18080     fncmdkey("&Next",1,1)
18100     fncmdkey("&Cancel",5,0,1)
18120   else if bon=3 then 
18140     fncmdkey("&Print",1,1)
18160     fncmdkey("&Cancel",5,0,1)
18180   else if bon=4 then 
18200     fncmdkey("&Save",1,1)
18220     fncmdkey("&Cancel",5,0,1)
18240   else if bon=5 then 
18260     fncmdkey("&Next",1,1)
18280     fncmdkey("&Cancel",5,0,1)
18300     fncmdkey("&Search",6)
18320   else if bon=6 then 
18340     fncmdkey("&Next",1,1)
18360     fncmdkey("&Back",2)
18380     fncmdkey("&Cancel",5,0,1)
18400   else if bon=7 then 
18420     fncmdkey("&Save",1,1)
18440     fncmdkey("&Delete",4)
18460     fncmdkey("&Cancel",5,0,1)
18480   else if bon=8 then 
18500     fncmdkey("&Print",1,1)
18520     fncmdkey("&Back", 2)
18540     fncmdkey("&Cancel",5,0,1)
18560   else if bon=11 then 
18580     fncmdkey("&Next",1,1)
18600     fncmdkey("&Finish",5,0,1)
18620   else if bon=13 then 
18640     fncmdkey("&Next",1,1)
18660     fncmdkey("&Add",2)
18680     fncmdkey("&Cancel",5,0,1)
18700   else if bon=14 then 
18720     fncmdkey("&Add",1)
18740     fncmdkey("E&dit",2,1)
18760     fncmdkey("&Print",4)
18780     fncmdkey("&Cancel",5,0,1)
18820   else if bon=15 then 
18840     fncmdkey("&Add",1,1)
18860     fncmdkey("&Cancel",5,0,1)
18880   else if bon=17 then 
18900     fncmdkey("&Next",1,1)
18920     fncmdkey("&Skip",2,0,1)
18940     fncmdkey("&Finish",3)
18960   else if bon=19 then 
18980     fncmdkey("&Next",1,1)
19000     fncmdkey("&Finish",2)
19020     fncmdkey("&Cancel",5,0,1)
19040   else if bon=21 then 
19060     fncmdkey("&Print",1,1)
19080     fncmdkey("&Search", 2)
19100     fncmdkey("&Cancel",5,0,1)
19120   else if bon=22 then 
19140     fncmdkey("&Next",1,1)
19160     fncmdkey("&Back",2)
19180     fncmdkey("&Cancel",5,0,1)
19200   else if bon=23 then 
19220     fncmdkey("&Add",1,1)
19240     fncmdkey("&Search",2)
19260     fncmdkey("&Finish",4)
19280     fncmdkey("&Cancel",5,0,1)
19300   else if bon=41 then 
19320     fncmdkey("&Ok",1,1,1)
19340   else if bon=52 then 
19360     fncmdkey("&Finish",5,1,1)
19380   else if bon=102 then 
19400     fncmdkey("&Print",1,1)
19420     fncmdkey("E&dit",3)
19440     fncmdkey("&Add",4)
19460     fncmdkey("&Delete",7)
19480     fncmdkey("&Refresh",6)
19500     fncmdkey("&Cancel",5,0,1)
19520   end if 
19540 fnend 
19560 def library fnbutton(lyne,ps,txt$*200,comkey; tt$*200,height,width,container,tabcon,default,cancel)
19580   ! adds a screen ace button
19600   ! mylen    button.width
19620   ! txt$     button.caption
19640   ! tt$      button.tooltiptext
19660   if ~setup then let fn_setup
19680   height=max(height,1) ! button height is at least 1
19700   if width=0 then width=len(txt$)
19720   setenv('control'&str$(fn_control_count),"BUTTON|"&str$(lyne)&"|"&str$(ps)&"|"&str$(height)&"|"&str$(width)&"|"&str$(comkey)&"|"&txt$&"|"&tt$&"|"&str$(default)&"|"&str$(cancel)&"|"&str$(container)&"|"&str$(tabcon)&"|")
19740 fnend 
19760 def library fnpicbut(lyne,ps,txt$*40,comkey,pic1$*100,btnh,btnw; pic2$*100,tt$*150,container,tabcon,default,cancel)
19762   if ~setup then let fn_setup
19780   ! mylen    button.width
19800   ! txt$     button.caption
19820   ! tt$      button.tooltiptext
19840   btnh=max(btnh,1) ! button height is at least 1
19860   if btnw=0 then btnw=len(txt$)
19880   setenv('control'&str$(fn_control_count),"PicBut|"&str$(lyne)&"|"&str$(ps)&"|"&str$(comkey)&"|"&str$(btnh)&"|"&str$(btnw)&"|"&str$(container)&"|"&str$(tabcon)&"|"&str$(default)&"|"&str$(cancel)&"|"&txt$&"|"&pic1$&"|"&pic2$&"|"&tt$&"|")
19900 fnend 
19920 IGNORE: continue 
19940 def library fndisplay_menu (mat _menu$,mat _program$,mat _status$;___,menu_string$*10000,index_)
19950   if ~setup then let fn_setup
19960   for index_=1 to udim(mat _menu$)
19980     menu_string$(inf:inf)=_menu$(index_)&'~~~'&_program$(index_)&'~~~'&_status$(index_)&'###'
20000   next index_
20020   setenv('control'&str$(fn_control_count),"menu|"&menu_string$(1:len(menu_string$)-3))
20040 fnend 
20041 def library fnclear_menu
20042   fn_clear_menu
20043 fnend 
20060 def fn_clear_menu
20080   mat _m$(0): mat _p$(0): mat _s$(0)
20100   display menu: mat _m$,mat _p$,mat _s$
20120 fnend 
20140 def library fnacs(sn$*100,unused,mat resp$,&ckey; startfield,close_on_exit,parent_none,disabled_background)
20160   if ~setup then let fn_setup
20180   dim txt$*201,path1$*300,tt$*400,tabline$*8019
20200   ! dim mycd$*256
20220   ! env$('Core_Program_Current')  dim prg$*80 ! current running program with relative to app.path
20240   dim cap$*128 ! caption / title bar text
20260   dim addtomask$*40
20280   ! on=1
20300   ! ****************
20320   acs=1
20340   tabcon=0
20360   ! if debug=1 then pr newpage
20380   ! ****************
20400   for j=1 to udim(mat resp$) : resp$(j)=rtrm$(resp$(j)) : next j ! was trim$ before 4/25/2017
20420   cap$=env$('Program_Caption')
20440   fn_get_flexhandle(1)
20460   ! do we even need this line - it screws up other things.
20480   ! does removing it screw anything up?
20500   ! yeah it screws things up to take it out - repetative flex grids
20540   fn_ace(sn$,unused,mat resp$,ckey,startfield,close_on_exit,parent_none,disabled_background)
20560   goto XIT
20580   ERTN: ! 
20600   ! execute "Config Console On" ! in ertn
20620   !   pr f "15,1,Cc 80,H,N": "Screen Ace 5 encountered an error."
20660   fnerror(program$,err,line,act$,"xit")
20680   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
20720   execute "List -"&str$(line)
20740   pause 
20860   ERTN_EXEC_ACT: ! 
20880   execute act$ : goto ERTN
20900   ! please note for program pause to work the way we want XIT: and fnend must be on the same line.!
20920 XIT: fnend 
20940 def fn_ace_init
20950   ace_io_count=ace_lyne_max=ace_column_max=grid_present=tmp_combo_count_for_read=0
20960   grid_index=date_boxes=respc=dropdown_menu_present=0 ! response counter
20970   date_fkey_base=2600 : file_select_fkey_base=2700 : txtbox_fkey=300
20980   mat ace_io$(0) : mat ace_typ$(0) : mat ace_resp$(0) : mat tabs(0,3)
20990   mat text_masks(0) : mat control$(0) : mat return_keys(0) : mat frames(0,4)
21000   mat date_fielddata(0,6) : mat file_select_data(0,2)
21010   grid_filter$=''
21020   scr_freeze
21030   resp_size=udim(mat resp$)
21040   button_win_width=0
21050   control_count=val(env$('control_count'))
21060   if udim(mat resp$)<control_count then mat resp$(control_count)
21070   mat ace_typ$(control_count)
21080   acs_win_rows=35 : setenv('acs_win_rows',str$(acs_win_rows))
21082   acs_win_cols=115 : setenv('acs_win_cols',str$(acs_win_cols))
21090 fnend 
21100 def fn_draw_windows
21110   acs_win_rows=max(acs_win_rows,ace_lyne_max+4) : setenv('acs_win_rows',str$(acs_win_rows)) ! in case 35 rows is not enough
21120     ! 
21130   acs_win_cols=max(acs_win_cols,ace_column_max+4) : setenv('acs_win_cols',str$(acs_win_cols)) ! in case 115 columns is not enough
21140   if acs_win_rows>35 or acs_win_cols>115 then 
21150     open #0: 'SRow=1,SCol=2,Rows='&str$(acs_win_rows)&",Cols="&str$(acs_win_cols)&',Picture='&env$('background_picture')&',border=S:[screen],N=[screen]',display,outin 
21160   end if 
21170   dim borderText$*256
21180   borderText$='ACS 5 '
21190   if session$(3:3)<>'1' then
21200     borderText$(inf:inf)='(Session '&session$(3:3)&') '
21202   end if
21210   if env$('CurSystem')=cap$ then ! pr border:
21220     borderText$(inf:inf)='- '&env$('CurSystem')
21230   else 
21240     borderText$(inf:inf)='- '&env$('CurSystem')&' - '&cap$
21250   end if 
21260   pr #0, border: borderText$
21270     ! 
21280   fn_company_name(0,acs_win_cols) ! fn_company_name(0,acs_win_cols,trim$(cap$(1:pos(cap$,'(')-1))) ! fnSystemName$(cursys$))
21290     ! 
21300   if not grid_present then 
21310     row=ceil((acs_win_rows-ace_lyne_max)/2 )
21320     col=ceil((acs_win_cols-ace_column_max)/2 )
21330     rows=ace_lyne_max
21340     cols=max(ace_column_max+2,button_win_width+2)
21350   else 
21360     row= 3
21370     col= 2
21380     rows=acs_win_rows-6
21390     cols=acs_win_cols-2
21400   end if 
21410   if parent_none then 
21420     !     if disabled_background then open #disable_win=fngethandle: "srow=2,scol=2,rows="&str$(acs_win_rows-2)&",cols="&str$(acs_win_cols-2)&",picture=S:\Core\disable.png:TILE",display,output
21430     !     if disabled_background then open #disable_win=fngethandle: "srow=1,scol=1,rows="&str$(acs_win_rows+2)&",cols="&str$(acs_win_cols+1)&",border=none,picture=S:\Core\disable.png:TILE",display,output
21440     if disabled_background then let fn_backgrounddisable(1)
21450     open #acs_win:=fngethandle: "SRow="&str$(row)&",SCol="&str$(col)&",Rows="&str$(rows+3)&",Cols="&str$(cols+2)&",parent=none,Caption="&cap$&",border=S:[screen],N=[screen]",display,outin 
21460     open #button_win:=fngethandle: "SRow="&str$(rows+2)&",SCol=2,Rows=1,Cols="&str$(cols)&",parent="&str$(acs_win)&",border=S:[screen],N=[screen]",display,outin 
21470   else 
21480     open #acs_win:=fngethandle: "SRow="&str$(row)&",SCol="&str$(col)&",Rows="&str$(rows)&",Cols="&str$(cols)&",parent=0,Caption="&cap$&",border=S:[screen],N=[screen]",display,outin 
21490     open #button_win:=fngethandle: "SRow="&str$(row+rows+2)&",SCol="&str$(col)&",Rows=1,Cols="&str$(cols)&",parent=0,border=S:[screen],N=[screen]",display,outin 
21500   end if 
21510   ace_cmdkey_ps=cols-button_win_width-1
21520 fnend 
21530 def fn_default_cmb_options
21540   dim tmp_combo_key$(1)*81,tmp_combo_item$(1)*81
21550   for tmp_combo_key_item=1 to tmp_combo_count_for_set
21560     str2mat( env$('tmp_combo'&str$(tmp_combo_key_item)&'_key'),mat tmp_combo_key$,'|')
21570     str2mat( env$('tmp_combo'&str$(tmp_combo_key_item)),mat tmp_combo_item$,'|')
21580     ! 
21590     tck_response_item=val(env$('tmp_combo'&str$(tmp_combo_key_item)&'_response_item'))
21600     tck_which=srch(mat tmp_combo_key$,trim$(resp$(tck_response_item)))
21610     if tck_which>0 and tck_which<=udim(mat tmp_combo_item$) then 
21620       resp$(tck_response_item)=tmp_combo_item$(tck_which)
21630     end if 
21640   next tmp_combo_key_item
21650 fnend 
22200 def fn_set_controls
22220   dim control$(1)*5095
22240   for index_=1 to control_count
22260     str2mat(env$('control'&str$(index_)),mat control$,"|")
22280     ace_typ$(index_)=typ$=uprc$(control$(1))
22300     if typ$="LABEL" then 
22320       fn_ace_rd_label
22340     else if typ$="TEXT" then 
22360       fn_ace_rd_text
22380     else if typ$="COMBOA" then 
22400       fn_ace_rd_combo('A')
22420     else if typ$="COMBOF" then 
22440       fn_ace_rd_combo('F')
22460     else if typ$="CHECK" then 
22480       fn_ace_rd_check
22500     else if typ$="OPTION" then 
22520       fn_ace_rd_option
22540     else if typ$="FLEX" then 
22560       fn_ace_rd_flex
22580     else if typ$="BUTTON" then 
22600       fn_ace_rd_button
22620     else if typ$="PICTURE" then 
22640       fn_ace_rd_pic
22660     else if typ$="CMDKEY" then 
22680       fn_ace_rd_cmdkey
22700     else if typ$="PICBUT" then 
22720       fn_ace_rd_picbut
22740     else if typ$="MULTILINE" then 
22760       fn_ace_rd_multiline
22780     else if typ$="FRAME" then 
22800       fn_ace_rd_frame
22820     else if typ$="TAB" then 
22840       fn_ace_rd_tab
22860     else if typ$='MENU' then 
22880       fn_ace_rd_menu
22900     end if 
22920   next index_
22940 fnend 
22960 def fn_equalize_resp_arrays
22980   respc=0
23000   dim ace_resp$(1)*1000, grid_filter$*1000
23020   if ace_io_count>udim(mat resp$) then mat resp$(ace_io_count)
23040   if ace_io_count then 
23060     if grid_present then 
23080       resp_size+=1
23100       grid_index=fn_grid_index
23120       mat ace_resp$(ace_io_count)
23140       if grid_index=1 then 
23160         mat ace_resp$(2:ace_io_count)=resp$(1:ace_io_count-1)
23180       else if grid_index=ace_io_count then 
23200         mat ace_resp$(1:ace_io_count-1)=resp$(1:ace_io_count-1)
23220       else 
23240         mat ace_resp$(1:grid_index-1)=resp$(1:grid_index-1) : mat ace_resp$(grid_index+1:ace_io_count)=resp$(grid_index:ace_io_count-1)
23260       end if 
23280     else 
23300       mat ace_resp$(ace_io_count)=resp$(1:ace_io_count)
23320     end if 
23340   end if 
23360 fnend 
23380 def fn_string_array_empty (mat arr$; ___,index_,nonempty)
23400   for index_=1 to udim(mat arr$)
23420     if trim$(arr$(index_))<>'' then 
23440       nonempty=1
23460     end if 
23480   next index_
23500   fn_string_array_empty=~ nonempty
23520 fnend 
23540 def fn_validate_grid_selection
23560   grid_selection_valid=1
23580   if fkey=0 or (fkey=201 and pos(ace_io$(curfld),"list ")) or fkey=default_button_fkey then 
23600     if grid_present then 
23620       if row_count then 
23640         if udim(ace_io$)=2 then 
23660           if fn_string_array_empty(mat ace_resp$) then 
23680             msgbox("Please, click on a row to select it and repeat your attempt.")
23700             grid_selection_valid=0
23720           end if 
23740         else 
23760           if fn_string_array_empty(mat grid_row$) then 
23780             msgbox("Please, click on a row to select it and repeat your attempt.")
23800             grid_selection_valid=0
23820           end if 
23840         end if 
23860       end if 
23880     end if 
23900   end if 
23920   fn_validate_grid_selection=grid_selection_valid
23940 fnend 
23960 def fn_process_user_input
23980   goto_main_input=0
24000   if fkey=0 or (fkey=201 and pos(ace_io$(curfld),"list ")) then 
24020     fkey(default_button_fkey) ! activate the default button when enter is pressed
24040     ckey=fkey
24060   else if fkey=2501 then 
24080     fn_export_grid
24100     fkey(-1) : goto_main_input=1
24120   else if fkey=2502 then 
24140     fn_print_grid
24160     fkey(-1) : goto_main_input=1
24180     !   else if fkey=2503 then
24200     !     pr f gridspec$&",sort": 1
24220     !     fkey(-1) : goto_main_input=1
24240   else if fkey > date_fkey_base and fkey < date_fkey_base+100 then 
24260     _date$=ace_resp$( date_fielddata(fkey-date_fkey_base,5))
24300     row=date_fielddata(fkey-date_fkey_base,2)
24320     column=date_fielddata(fkey-date_fkey_base,3)+date_fielddata(fkey-date_fkey_base,4)+1
24340     ace_resp$(date_fielddata(fkey-date_fkey_base,5))=fndate_picker$ (_date$,'mdy',row,column)
24360     fkey(-1) : goto_main_input=1
24380   else if fkey > file_select_fkey_base and fkey < file_select_fkey_base+100 then 
24400     fn_selectfile( ace_resp$(file_select_data(fkey-file_select_fkey_base,1) ), file_select_data(fkey-file_select_fkey_base,2))
24420     fkey(-1) : goto_main_input=1
24440   else if fkey=93 then 
24460     if env$('ACSDeveloper')<>'' then let setenv('ExitNow','yes')
24470     ckey=fkey_cancel
24480   else if fkey=98 then 
24500     ckey=fkey
24520   else if fkey=99 then 
24540     ckey=fkey_cancel
24542   else if fkey=1504 then ! 2504 then
24544     if cap$='Select Company' then 
24546       help_cursys$='co'
24548     else 
24550       help_cursys$=lwrc$(env$('CurSys'))
24552     end if 
24580     execute 'system -M start http://planetacs.net/help/'&help_cursys$&'/'&srep$(trim$(cap$),' ','%20')&'.html'
24600     goto_main_input=1
24620   else if fkey=1505 then 
24640     fn_program_properties
24642     goto_main_input=1
24660   else if (fkey=105 or fkey=124 or fkey=106 or fkey=125) and (pos(ace_io$(curfld),"list ") or pos(ace_io$(curfld),"list ")) then 
24680     goto_main_input=1
24700     if fkey=105 or fkey=124 then let setenv('current_grid_row','1')
24720   else if not srch(mat return_keys,fkey)>0 or fkey=92 or fkey=208 or fkey=txtbox_fkey then ! this means user switched tabs (fkey 92) or picked a value from a combo box (fkey 208)
24740     goto_main_input=1
24760   else 
24780     ckey=fkey
24800   end if 
24820   fn_process_user_input=goto_main_input
24840 fnend 
24860 def fn_reformat_user_input
24862   !   if udim(resp$)<ace_io_count then mat resp$(ace_io_count)
24864   !   if udim(ace_resp$)<ace_io_count then mat ace_resp$(ace_io_count)
24880   for ace_resp_item=1 to ace_io_count
24882     if ace_resp_item<=udim(mat ace_resp$) and ace_resp_item<=udim(mat resp$) then 
24900       if pos(ace_io$(ace_resp_item),'check ')>0 or pos(ace_io$(ace_resp_item),'radio ')>0 then 
24920         if ace_resp$(ace_resp_item)(1:1)='^' then 
24940           resp$(ace_resp_item)='True'
24960         else 
24980           resp$(ace_resp_item)='False'
25000         end if 
25020       else if date_index := fn_is_date(mat date_fielddata,ace_resp_item) > 0 then 
25040         _mask=date_fielddata(date_index,6)
25060         if _mask=1 then 
25080           date_format$="mdy"
25100         else if _mask=2 then 
25120           date_format$="mdcy"
25140         else if _mask=3 then 
25160           date_format$="cymd"
25180         else if _mask=4 then 
25200           date_format$="dmcy"
25220         else if _mask=5 then 
25240           date_format$="dmy"
25260         end if 
25280         if date_format$<>'mdy' then 
25300           _days=days(lpad$(trim$(ace_resp$(ace_resp_item)),6,'0'),'mdy')
25320           resp$(ace_resp_item)=date$(_days,date_format$)
25340         else 
25360           resp$(ace_resp_item)=ace_resp$(ace_resp_item)
25380         end if 
25400       else 
25420         resp$(ace_resp_item)=rtrm$(ace_resp$(ace_resp_item))
25440       end if 
25450     end if 
25460   next ace_resp_item
25480 fnend 
25560 def fn_error_redirection
25580   if env$("ACSDeveloper")<>"" then 
25600     on soflow goto ERTN
25620   else 
25640     on soflow ignore 
25660   end if 
25680 fnend 
25700 def fn_grid_index(;___,index_,grid_idx)
25720   for index_=1 to udim(mat ace_io$)
25740     if pos(ace_io$(index_),'list ') then 
25760       grid_idx=index_
25780     end if 
25800   next index_
25820   fn_grid_index=grid_idx
25840 fnend 
26000 def fn_main_input
26010   if grid_present then 
26020     grid_index=fn_grid_index
26030     PopulateGrid: ! 
26040     current_grid_row=val( env$('current_grid_row')) ! if current_grid_row then pr 'current_grid_row=';current_grid_row : pause
26050     if current_grid_row then 
26060       curfld(grid_index,current_grid_row)
26070       setenv('current_grid_row','')
26080       input fields gridspec$&",rowcnt,all,nowait": grid_rows
26090     else 
26100       curfld(grid_index,row_count)
26110     end if 
26120     !  
26130     grid_filter$=''
26140     dim grid_row$(1)*10000
26150     if not grid_populated then ! file_nonempty then ! if no rows have been populated, we have to create one
26160       pr f gridspec$&",headers,[gridheaders]" : (mat _headings$,mat _widths,mat _forms$)
26170       mat grid_row$(udim(ace_resp$))=("")
26180       pr f gridspec$&",=": mat grid_row$
26190     end if 
26200     ! 
26210     ! 
26220     ! mat ace_io$(ace_io_count)
26230     ! mat ace_resp$(ace_io_count)
26240     if udim(ace_io$)=2 then ! this is if the grid is the only control
26250       curfld(2) !  if ~current_grid_row then let curfld(2) ! if grid is first control than set the focus to the filter box
26260       rinput fields mat ace_io$: mat ace_resp$, grid_filter$ error ignore ! error MainInput886Avoidance
26270       current_grid_row=0
26280       if udim(ace_resp$)>1 then 
26290         mat ace_resp$(1:udim(ace_resp$)-1)=ace_resp$(2:udim(ace_resp$))
26300         mat ace_resp$(udim(ace_resp$)-1)
26310       else 
26320         mat ace_resp$(ace_io_count)=resp$(1:ace_io_count)
26330       end if 
26340     else ! this is if the grid is not the only control
26350       mat grid_row$(udim(_headings$))=("") ! this will contain the selected row data
26360       if not udim(ace_io$)-1=udim(ace_resp$) then 
26370         mat ace_resp$(udim(mat ace_resp$)-1) ! this will contain the rest of the non-grid data
26380         ace_io_count-=1
26390       end if 
26400       if grid_index=1 then ! the grid and filter are the FIRST two controls in ace_io$
26410         curfld(2) ! if grid is first control than set the focus to the filter box
26420         rinput fields mat ace_io$: mat grid_row$, grid_filter$, mat ace_resp$(2:udim(ace_resp$)) error IGNORE ! conv CONV_HANDLER
26430       else if grid_index=udim(ace_resp$) then ! the grid and filter are the LAST two controls in ace_io$
26440         curfld(1)
26450         rinput fields mat ace_io$: mat ace_resp$(1:udim(ace_resp$)-1), mat grid_row$, grid_filter$ conv CONV_HANDLER error ignore ! error IGNORE_886
26460       else ! the grid and filter are in the middle of other controls in ace_io$
26470         curfld(1)
26480         rinput fields mat ace_io$: mat ace_resp$(1:grid_index-1), mat grid_row$, grid_filter$, mat ace_resp$(grid_index+1:udim(ace_resp$)) conv CONV_HANDLER error IGNORE
26490       end if 
26500       if udim(grid_row$)>1 then ace_resp$(grid_index)=grid_row$(2) else ace_resp$(grid_index)=""
26510     end if 
26520   else if ace_io_count <> 0 then 
26530     if fn_all_fields_protected(mat ace_io$) then 
26540       pr f mat ace_io$: mat ace_resp$
26550       input #0, fields str$(acs_win_rows)&","&str$(acs_win_cols)&',C 1' : dummy$ ! this is when a screen has no inputs, only labels and/or buttons
26560     else 
26570       rinput fields mat ace_io$: mat ace_resp$ conv CONV_HANDLER
26580     end if 
26590   else 
26600     input #acs_win, fields str$(rows)&','&str$(cols)&',C 1' : dummy$ ! this is when a screen has no inputs, only labels and/or buttons
26610     ! .!input #0, fields str$(acs_win_rows)&","&str$(acs_win_cols)&',C 1' : dummy$ ! this is when a screen has no inputs, only labels and/or buttons
26620   end if 
26630 fnend 
26940 def fn_ace(sn$*100, unused,mat resp$, &ckey;startfield, close_on_exit, parent_none,background)
26960   if env$('ExitNow')='yes' then 
26980     ckey=fkey_cancel
26982     goto ACE_COMPLETE
27000   end if 
27020   dim ace_io$(1)*255
27040   fn_ace_init
27060   fn_window_size
27080   fn_draw_windows
27120   fn_error_redirection
27140   fn_set_controls
27160   fn_default_cmb_options
27180   fn_clear_env
27200   fn_equalize_resp_arrays
27220   if not dropdown_menu_present then let fn_clear_menu
27240   MAIN_INPUT: ! 
27260   fn_main_input
27280   if fn_process_user_input or not fn_validate_grid_selection then 
27300     goto MAIN_INPUT
27320   end if 
27340   if env$('ExitNow')='yes' then 
27360     ckey=fkey_cancel
27362     goto ACE_COMPLETE
27380   end if 
27400   if ckey<>fkey_cancel then 
27420     if not fn_validate_fields then 
27440       goto MAIN_INPUT
27460     end if 
27480   end if 
27500   fn_reformat_user_input
27520   on soflow system 
27540   if udim(mat resp$)<resp_size then 
27560     mat resp$(resp_size)
27580   end if 
27600   ACE_COMPLETE: ! 
27620   fkey(-1)
27640   fn_close_windows ! :display menu:
27650   if disabled_background then let fn_backgrounddisable(0)
27660 fnend 
27680 def fn_close_windows
27700   if file(acs_win)<>-1 then close #acs_win: 
27720   if file(button_win)<>-1 then close #button_win: 
27740   if file(disable_win)<>-1 then close #disable_win: 
27760   if acs_win_cols then 
27780     pr #0, fields "1,"&str$(acs_win_cols-5)&",C 2;1,"&str$(acs_win_cols-2)&",C 2": rpt$(' ',2), rpt$(' ',2)
27800   end if 
27820 fnend 
27840 CONV_HANDLER: ! r:
27860   dim message$(1)*255, response$*255,temp_io$(1)*255
27880   bad_field=cnt+1
27900   message$(1)='You have entered an incorrect value at field number '&str$(bad_field)
27920   mat temp_io$(udim(ace_io$))=ace_io$
27940   fnmsgbox(mat message$, response$, "Error!",0)
27950   fnpause
27960   mat ace_resp$(udim(resp$))=resp$
27980   mat ace_io$(udim(temp_io$))=temp_io$
28000   curfld(bad_field)
28020 retry  ! /r
28040 def fn_is_date (mat date_data,ace_item;___,index_,isdate)
28060   isdate=0
28080   for index_=1 to udim(mat date_data)
28100     if date_data(index_,5)=ace_item then 
28120       isdate=index_ : goto DATESEARCH_COMPLETE
28140     end if 
28160   next index_
28180   DATESEARCH_COMPLETE: ! 
28200   fn_is_date=isdate
28220 fnend 
28240 def fn_program_properties
28260   library 'S:\Core\Library': fnprogram_properties
28300   dim temp_resp$(1)*512,temp_ace_resp$(1)*512
28320   mat temp_io$(udim(ace_io$))=ace_io$
28340   mat temp_resp$(udim(resp$))=resp$
28360   mat temp_ace_resp$(udim(ace_resp$))=ace_resp$
28380   temp_io_count=ace_io_count
28400   fnprogram_properties
28420   mat ace_io$ (udim(temp_io$))=temp_io$
28440   mat resp$(udim(temp_resp$))=temp_resp$
28460   mat ace_resp$(udim(temp_ace_resp$))=temp_ace_resp$
28480   ace_io_count=temp_io_count
28500 fnend 
28520 def fn_all_fields_protected(mat ace_io$)
28540   all_fields_protected=1
28560   for index_=1 to udim(mat ace_io$)
28580     if not pos(ace_io$(index_),',P') then all_fields_protected=0
28600   next index_
28620   fn_all_fields_protected=all_fields_protected
28640 fnend 
28660 def fn_clear_env
28670   for index_=1 to control_count
28680     setenv('control'&str$(index_),"")
28690     !     setenv('combo'&str$(index_),"")
28700   next index_
28710   for tmp_combo_item=1 to tmp_combo_count_for_set
28720     setenv('tmp_combo'&str$(tmp_combo_item),'')
28722     setenv('tmp_combo'&str$(tmp_combo_count_for_read)&'_response_item','')
28724     setenv('tmp_combo'&str$(tmp_combo_count_for_read)&'_key','')
28730   next tmp_combo_item
28740   tmp_combo_count_for_set=tmp_combo_count_for_read=0
28750   setenv('control_count',0)
28760 fnend 
28800 def fn_window_size
28820   !   frame_or_tab_present=0
28840   for index_=1 to control_count
28860     str2mat(env$('control'&str$(index_)),mat control$,"|")
28880     ace_typ$(index_)=typ$=uprc$(control$(1))
28900     if typ$="LABEL" then 
28920       ace_lyne_max=max(ace_lyne_max,val(control$(2)))
28940       ace_column_max=max(ace_column_max, val(control$(3))+max(val(control$(4)),len(control$(6))))
28960     else if typ$="TEXT" or typ$="COMBOA" or typ$="COMBOF" then 
28980       ace_lyne_max=max(ace_lyne_max,val(control$(2)))
29000       ace_column_max=max( ace_column_max, val(control$(3))+val(control$(4)) )
29020     else if typ$="CHECK" or typ$="OPTION" then 
29040       ace_lyne_max=max(ace_lyne_max,val(control$(2)))
29042      ! if index_=5 then pr 'it is 5' : pause
29060       ace_column_max=max( ace_column_max, val(control$(3))+len(control$(5))+4) ! max( ace_column_max, val(control$(3))+len(trim$(control$(5)))+4)
29080     else if typ$="FLEX" or typ$="MULTILINE" or typ$="FRAME" or typ$="TAB" then 
29100       if typ$="FLEX" then grid_present=1
29120       ace_lyne_max=max(ace_lyne_max,val(control$(2))+val(control$(4)))
29140       if typ$="FRAME" or typ$="TAB" and (not save_bottom or val(control$(2))+val(control$(4))>save_bottom) then ace_lyne_max += 1
29160       ace_column_max=max( ace_column_max, val(control$(3))+val(control$(5)) )
29180       save_bottom=val(control$(2))+val(control$(4))
29200     else if typ$="CMDKEY" then 
29220       button_win_width +=(len(control$(2))+1)
29240     else if typ$="BUTTON" then 
29260       ace_lyne_max=max(ace_lyne_max,val(control$(2)))
29280       ace_column_max=max( ace_column_max, val(control$(3))+val(control$(5)) )
29300     else if typ$="PICTURE" then 
29320       ace_lyne_max=max(ace_lyne_max,val(control$(2))+val(control$(4)))
29340       ace_column_max=max( ace_column_max, val(control$(3))+val(control$(6)) )
29360     else if typ$="PICBUT" then 
29380       ace_lyne_max=max(ace_lyne_max,val(control$(2)))
29400       ace_column_max=max( ace_column_max, val(control$(3))+val(control$(6)) )
29420     else if typ$="MENU" then 
29440       dropdown_menu_present=1
29460     end if 
29480   next index_
29500 fnend 
29520 def fn_validate_fields(;___,found_invalid)
29540   for _idx=1 to udim(text_masks)
29560     if text_masks(_idx)>=1 and text_masks(_idx)<=5 then 
29580       if ace_resp$(_idx)<>'0' then 
29600         if not fn_validate_mdy(ace_resp$(_idx)) then 
29620           if val(resp$(_idx)) then 
29640             msgbox('Date '&ace_resp$(_idx)&' at field #'&str$(curfld)&' is invalid. Previous value of '&resp$(_idx)&' will be restored')
29660             ace_resp$(_idx)=resp$(_idx)
29680           else 
29700             msgbox('Date '&ace_resp$(_idx)&' at field #'&str$(curfld)&' is invalid.')
29720           end if 
29740           found_invalid=1
29760           curfld(_idx) : fkey(-1)
29780         end if 
29800       end if 
29820     else if text_masks(_idx)>1000 and (trim$(ace_resp$(_idx))='' or trim$(ace_resp$(_idx))='0') then 
29840       msgbox('Field '&str$(_idx)&' is required.')
29860       found_invalid=1
29880       curfld(_idx) : fkey(-1)
29882       !     else if text_masks(_idx)>1000 then
29884       !       pr 'field is required - and is not blank, which is good, but it might be a date field.'
29886       !       pr 'text_masks(_idx)=';text_masks(_idx)
29888       !       pr 'ace_resp$(_idx)=';ace_resp$(_idx) : pause
29890       !       curfld(_idx) : fkey(-1)
29892       !       found_invalid=1
29900     end if 
29920   next _idx
29940   fn_validate_fields=~ found_invalid
29960 fnend 
29980 def fn_validate_mdy (_date$;separator$,___,month,day,year)
30000   fn_validate_mdy=1
30020   _date$=lpad$(_date$,6,'0')
30040   month=val(_date$(1:2))
30060   day=val(_date$(3:4))
30080   year=val(_date$(5:6))
30100   if not (fn_validate_month(month) and fn_validate_day(day,month,year) and fn_validate_year(year)) then let fn_validate_mdy=0
30120  ! FINISHEDVALIDATE_MDY: !
30140 fnend 
30160 def fn_validate_month(month)
30180   fn_validate_month=1
30200   if month < 1 or month > 12 then let fn_validate_month=0
30220 fnend 
30240 def fn_validate_day(day,month,year)
30260   fn_validate_day=1
30280   if day<1 or day>fn_days_in_month(month,year) then let fn_validate_day=0
30300 fnend 
30320 def fn_validate_year(year)
30340   fn_validate_year=1
30360   if year < 0 or year > 99 then let fn_validate_year=0
30380 fnend 
30400 def fn_days_in_month (month,year;___,daysinmonth)
30420   fn_days_in_month=date(days(date$(days(date$(str$(year)&lpad$(str$(month),2,"0")&"01"),"CCYYMMDD")+32,"CCYYMM01"),"CCYYMMDD")-1,"DD")
30440 fnend 
30460 def fn_ace_rd_menu (;___,index_,item_count)
30480   dim menu_items$(1)*1023,menu_sub_items$(1)*255,_menu$(1)*255,_program$(1)*255,_status$(1)*255
30500   str2mat(control$(2),mat menu_items$,'###')
30520   item_count=udim(mat menu_items$)
30540   mat _menu$(item_count)=(''): mat _program$(item_count)=(''): mat _status$(item_count)=('')
30560   for index_=1 to udim(mat menu_items$)
30580     str2mat(menu_items$(index_),mat menu_sub_items$,'~~~')
30600     _menu$(index_)=menu_sub_items$(1)
30620     _program$(index_)=menu_sub_items$(2)
30640     _status$(index_)=menu_sub_items$(3)
30660   next index_
30680   display menu: mat _menu$,mat _program$,mat _status$
30700 fnend 
30720 def fn_ace_rd_multiline
30740   respc+=1
30760   lyne=val(control$(2))
30780   ps=val(control$(3))
30800   height=val(control$(4))
30820   width=val(control$(5))
30840   tt$=control$(6)
30860   container=val(control$(7))
30880   tabcon=val(control$(8))
30900   fn_remove_crlf(resp$(respc))
30910    !   resp$(respc)=srep$(resp$(respc),'"','""') ! fn2quote(resp$(respc))
30920 fnend 
30940 def fn_ace_rd_picbut
30960   lyne=val(control$(2))
30980   ps=val(control$(3))
31000   comkey=val(control$(4))
31020   height=val(control$(5))
31040   width=val(control$(6))
31060   container=val(control$(7))
31080   tabcon=val(control$(8))
31100   default=val(control$(9))
31120   cancel=val(control$(10))
31140   txt$=control$(11)
31160   path1$=control$(12)
31180   ! path2$=control$(13)
31200   tt$=control$(14)
31220 fnend 
31240 def fn_ace_rd_cmdkey
31260   dim spec$*255
31280   spec$=''
31300   txt$=control$(2)
31320   mat return_keys(udim(return_keys)+1)
31340   returnkey=return_keys(udim(return_keys))=val(control$(3))
31360   default=val(control$(4))
31380   cancel=val(control$(5))
31400   if udim(control$)>=6 then 
31420     tt$=control$(6)
31440   end if 
31460   if cancel then let fkey_cancel=returnkey
31480   txt$=srep$(txt$,'&','') ! remove underlined letters...   would be nice to use them. xxx
31500   width=len(txt$)
31520   spec$='1,'&str$(ace_cmdkey_ps)&',CC '&str$(width)&',,B'&str$(returnkey)
31540   if default then 
31560     default_button_fkey=returnkey
31580     spec$='1,'&str$(ace_cmdkey_ps)&',CC '&str$(width)&',[buttons],B'&str$(returnkey)
31600     pr #0, fields "1,5,P 1/2,[buttons],"&str$(returnkey): "S:\Core\Icon\forward-icon.png" ioerr ignore
31610   else if cancel then 
31620     spec$='1,'&str$(ace_cmdkey_ps)&',CC '&str$(width)&',[buttoncancel],B'&str$(returnkey)
31630     if env$('tmp_acs_back_arrow')<>'' then 
31640       pr #0, fields "1,2,P 1/2,[buttons],"&str$(returnkey): env$('tmp_acs_back_arrow') ioerr ignore
31650     else 
31660       pr #0, fields "1,2,P 1/2,[buttons],"&str$(returnkey): "S:\Core\Icon\back-icon.png" ioerr ignore
31670     end if 
31680   else 
31700     spec$='1,'&str$(ace_cmdkey_ps)&',CC '&str$(width)&',,B'&str$(returnkey)
31720   end if 
31740   ! 
31760   dim _help$*255
31780   if tt$='' then 
31800     _help$="1;Press ["
31820     if default then 
31840       _help$(inf:inf)="Enter"
31860     else if cancel then 
31880       _help$(inf:inf)="Esc"
31900     else if returnkey >=2 and returnkey<=12 then ! because F1 will be used for Help
31920       _help$(inf:inf)="F"&str$(returnkey)
31940     else 
31960       _help$=''
31980     end if 
32000     if _help$<>'' then _help$(inf:inf)="] to "&txt$&";"
32020   else 
32040     _help$='1;'&tt$&';'
32060   end if 
32100   pr #button_win, fields spec$, help _help$: txt$
32120   ace_cmdkey_ps+=(width+2)
32140 fnend 
32160 def fn_ace_rd_tab
32180   lyne=val(control$(2))
32200   ps=val(control$(3))
32220   height=val(control$(4))
32240   width=val(control$(5))
32260   path1$=control$(6) & control$(7)
32280   open #tab_file:=fngethandle: "Name="&path1$,internal,outin 
32300   tabline$=""
32320   for j=1 to min(lrec(tab_file),99)
32340     read #tab_file,using "Form Pos 1,C 80": txt$
32360     tabline$=tabline$&trim$(txt$)&tab$
32380     open #tab:=fngethandle: 'srow='&str$(lyne+1)&',scol='&str$(ps+1)&',rows='&str$(height+1)&',cols='&str$(width+1)&',tab='&trim$(txt$)&',parent='&str$(acs_win)&',border=S:[screen],N=[screen]',display,outin 
32400     mat tabs(udim(tabs)+1,3)
32420     tabs(udim(tabs),1)=tab
32440     tabs(udim(tabs),2)=lyne+1
32460     tabs(udim(tabs),3)=ps+1
32480   next j
32500   close #tab_file: 
32520 fnend 
32540 def fn_ace_rd_frame
32560   lyne=val(control$(2))
32580   ps=val(control$(3))
32600   height=val(control$(4))
32620   width=val(control$(5))
32640   txt$=control$(6)
32660   tt$=control$(7)
32680   container=val(control$(8))
32700   tabcon=val(control$(9))
32720   height+=1
32740   txt$=trim$(txt$)
32760   ! txt$=srep$(txt$,'"','""') ! fn2quote(txt$)
32780   mat frames(udim(frames)+1,4)
32800   if tabcon then 
32820     open #frame:=fngethandle: 'srow='&str$(lyne+1)&',scol='&str$(ps+1)&',rows='&str$(height-1)&',cols='&str$(width)&',parent='&str$(tabs(tabcon,1))&',border=S:[screen],N=[screen],caption='&srep$(txt$,',','.'),display,outin 
32840     frames(udim(frames),4)=tabs(tabcon,1)
32860   else 
32880     open #frame:=fngethandle: 'srow='&str$(lyne+1)&',scol='&str$(ps+1)&',rows='&str$(height-1)&',cols='&str$(width)&',parent='&str$(acs_win)&',border=S:[screen],N=[screen],caption='&srep$(txt$,',','.'),display,outin 
32900   end if 
32920   frames(udim(frames),1)=frame
32940   frames(udim(frames),2)=lyne+1
32960   frames(udim(frames),3)=ps+1
32980 fnend 
33000 def fn_ace_rd_flex(;___,index_)
33040   lyne=val(control$(2))
33060   ps=val(control$(3))
33080   height= rows - lyne ! val(control$(4))
33100   width= cols - ps -2 ! val(control$(5))
33120   seltype=val(control$(7))
33140   sorttype=val(control$(8))
33160   path1$=control$(9)
33180   hdr_count=val(control$(10))
33200   container=val(control$(11))
33220   tabcon=val(control$(12))
33240   dim _headings$(1)*1000,_line$*10000,_chunks$(1)*1000,_forms$(1)*1000,filterspec$*255,gridspec$*255,loading_spec$*50
33250   ! pr env$('temp')&'\'&trim$(path1$)&'.hdr' : pause
33260   open #grid_headers:=fngethandle: 'Name='&env$('temp')&'\'&trim$(path1$)&'.hdr',display,input 
33280   linput #grid_headers: _line$
33300   str2mat(_line$,mat _headings$,tab$)
33320   linput #grid_headers: _line$
33340   str2mat(_line$,mat _mask$,tab$)
33360   mat _mask$(udim(_headings$))
33380   close #grid_headers: 
33400   file_nonempty=fn_gridform(mat _widths,mat _forms$,mat _mask$,mat _headings$)
33420   if not file_nonempty then 
33440     for col_index_=2 to udim(mat _headings$)
33460       _widths (col_index_)=len(_headings$(col_index_))
33480       _forms$(col_index_)="C "&str$(_widths (col_index_))
33500     next col_index_
33520   end if 
33540   ! this is done when there is a grid to the right of a frame, so they don't overlap
33560   if udim(frames) and not container and ps > 2 then 
33580     ps+=1
33600   end if 
33620   ! 
33640   filterspec$=str$(lyne)&","&str$(ps)&","&str$(width+2)&"/filter "&str$(width)&",[textboxes],"&str$(lyne+1)&","&str$(ps)&",1,word"
33660   ! 
33680   if not container and not tabcon then 
33700     gridspec$=str$(lyne+1)&","&str$(ps)&",list "&str$(height-1)&"/"&str$(width+2)
33720     loading_spec$=str$(lyne + height)&","&str$(ps)&",C "
33740   else 
33760     gridspec$=str$(lyne+1)&","&str$(ps)&",list "&str$(height-2)&"/"&str$(width+2)
33780     loading_spec$=str$(lyne + height-1)&","&str$(ps)&",C "
33800   end if 
33820   ! 
33840   if container then 
33860     window_prefix$='#'&str$(frames(container,1))&','
33880   else if tabcon then 
33900     window_prefix$='#'&str$(tabs(tabcon,1))&','
33920   else 
33940     window_prefix$='#'&str$(acs_win)&','
33960   end if 
33980   filterspec$(0:0)=window_prefix$
34000   gridspec$(0:0)=window_prefix$
34020   loading_spec$(0:0)=window_prefix$
34040   ! 
34060   pr f gridspec$&",headers,[gridheaders]" : (mat _headings$,mat _widths,mat _forms$)
34080   open #grid_data:=fngethandle: 'Name='&env$('temp')&'\'&trim$(path1$)&'[SESSION].tmp',display,input 
34100   clearflag$="="
34120   ! 
34140   dim long_row$(1)*1024
34160   rows=2000
34180   mat long_row$(rows * udim(_headings$))
34200   row_count=1 : record_count=1
34220   printed=0
34240   ! 
34260   scr_thaw
34280   ! 
34300   fn_alpha_mask_indices (mat alpha_mask_indices)
34340   do while file_nonempty
34360     linput #grid_data: _line$ eof ignore
34380     ! remove this line after you figure out how to addtomask$ negative numbers to the #pic spec
34400     !   _line$=srep$(_line$,'-','')
34420     ! 
34440     if file(grid_data)<>0 then exit do 
34460     record_count += 1
34480     if record_count=30000 then 
34500       value= msgbox( "30,000 records have been loaded. Do you wish to continue loading?", "Message", "YN", "INF")
34520       if value=3 then goto GRID_DATA_LOAD_COMPLETE
34540     end if 
34560     ! 
34580     str2mat(_line$,mat _chunks$,tab$)
34600     if udim(_chunks$)<>udim(_headings$)-1 then ! truncate extra columns, which are there by mistake
34620       mat _chunks$(udim(_headings$)-1 )
34640     end if 
34660     ! 
34680     ! if env$('ACSDeveloper')<>'' then
34700     mat2str(mat _chunks$,_line$," ")
34720     ! else 
34740     !   _line$=''
34760     !   for index_=1 to udim(mat _chunks$)
34780     !     if val(_mask$(index_)) < 1000 then
34800     !       _line$(inf:inf)=_chunks$(index_)&' '
34820     !     end if
34840     !   next index_
34860     ! end if
34880     ! 
34900     for index_=1 to udim(mat alpha_mask_indices)
34920       cell_value=val(_chunks$(alpha_mask_indices(index_))) conv BAD_NUMERIC_CELL
34940     next index_
34960     goto CREATE_FILTER_COLUMN
34980     BAD_NUMERIC_CELL: ! 
35000     _chunks$(alpha_mask_indices(index_))='0' : retry 
35020     ! 
35040     CREATE_FILTER_COLUMN: ! 
35060     mat _chunks$(udim(_chunks$)+1)
35080     mat _chunks$(2:udim(_chunks$))=_chunks$(1:udim(_chunks$)-1)
35100     _chunks$(1)=_line$
35120     ! 
35140     ! CHECK_JULIAN_DATES: ! Convert dates to julain format for BR internal date specs
35160     dim datemask$,masknumber
35180     for index_=1 to udim(mat _mask$)
35200       masknumber=val(_mask$(index_)) conv NOT_JULIAN_DATE
35220       if masknumber>=1000 then masknumber-=1000
35240       if masknumber>=01 and masknumber<=05 then 
35260     !         if masknumber=01 then !    date format : mm/dd/yy
35280     !           datemask$="ccyymmdd" ! "mmddyy" ! Expected programs sending dates like mddyy
35300     !         else if masknumber=02 then !    date format : mm/dd/ccyy
35320     !           datemask$="ccyymmdd" ! "mmddccyy" ! Expected programs sending dates like mmddccyy
35340     !         else if masknumber=03 then !    date format : ccyy/mm/dd
35360     !           datemask$="ccyymmdd"
35380     !         else if masknumber=04 then !    date format : dd/mm/ccyy
35400     !           datemask$="ccyymmdd" ! "ddmmccyy"
35420     !         else if masknumber=05 then !    date format : dd/mm/yy
35440         datemask$="ccyymmdd" ! "ddmmyy"
35460     !         end if
35480     !         _chunks$(index_+1)=srep$(lpad$(_chunks$(index_+1),len(datemask$))," ","0") ! pause ! lpad with zeros to the right size
35482         quick_len=len(trim$(srep$(_chunks$(index_+1),'/',''))) ! pr quick_len
35483         if quick_len=5 then quick_len=6 : _chunks$(index_+1)='0'&trim$(_chunks$(index_+1))
35484         if quick_len=6 then _chunks$(index_+1)=date$(days(_chunks$(index_+1),'mmddyy'),'ccyymmdd') ! datemask$='mmddyy' else datemask$="ccyymmdd"! =str$(days(_chunks$(index_+1),datemask$)) ! Convert to julain date according to mask for data in expected format
35498     !         pr _chunks$(index_+1),quick_len : pause ! =date$(days(_chunks$(index_+1),'mmddyy'),'ccyymmdd') ! datemask$='mmddyy' else datemask$="ccyymmdd"! =str$(days(_chunks$(index_+1),datemask$)) ! Convert to julain date according to mask for data in expected format
35500         _chunks$(index_+1)=str$(days(_chunks$(index_+1),datemask$)) ! Convert to julain date according to mask for data in expected format
35520       end if 
35540       NOT_JULIAN_DATE: ! Not a julian date field, leave it alone
35560     next index_
35580     if row_count <= rows then 
35600       mat long_row$(1+(row_count-1)*udim(_chunks$):row_count*udim(_chunks$))= _chunks$
35620       if row_count=rows then 
35640         pr f gridspec$&","&clearflag$&"L": mat long_row$
35660         pr f loading_spec$ : "Loading... Please wait"
35680         scr_freeze
35700         clearflag$="+"
35720         mat long_row$=('')
35740         row_count=1
35760         printed=1
35780       else 
35800         row_count += 1
35820       end if 
35840     end if 
35860   loop 
35880   row_count -= 1
35900   if row_count<>0 then 
35920     if not printed then 
35940       pr f gridspec$&",=L": mat long_row$(1:(row_count)*udim(_chunks$))
35960     else if row_count <> rows then 
35980       pr f gridspec$&",+L": mat long_row$(1:(row_count)*udim(_chunks$))
36000     end if 
36020   end if 
36040   GRID_DATA_LOAD_COMPLETE: ! 
36060   ! clear the "Loading..." message
36080   pr f loading_spec$ : rpt$(" ",30)
36100   ! 
36120   close #grid_data: 
36140   fn_ace_io_add(gridspec$&",row,selone")
36160   fn_ace_io_add(filterspec$)
36180   !   filter_index=ace_io_count
36200   ! 
36220   if not container and not tabcon then 
36240     srow$=str$(lyne+height)
36260   else 
36280     srow$=str$(lyne+height-1)
36300   end if 
36320   ! 
36340   pr f window_prefix$&srow$&","&str$(ps+00)&",CC 7,,B2501": "Export"
36360   pr f window_prefix$&srow$&","&str$(ps+08)&",CC 7,,B2502": "Print" ! if env$('ACSDeveloper')<>'' then
36380   !   pr f window_prefix$&srow$&","&str$(ps+16)&",CC 7,,B2503": "Reset"
36400 fnend 
36420 def fn_alpha_mask_indices (mat alpha_mask_indices;___,index_,mask)
36440   mat alpha_mask_indices(0)
36460   for index_=1 to udim(mat _mask$)
36480     mask=val(_mask$(index_)) conv ignore
36500     if mask >=1000 then mask-=1000
36520     if mask<>0 and mask<>80 and mask<>81 then ! is numeric
36540       mat alpha_mask_indices(udim(mat alpha_mask_indices)+1)
36560       alpha_mask_indices(udim(mat alpha_mask_indices))=index_
36580     end if 
36600   next index_
36620 fnend 
36640 def fn_gridform(mat _widths,mat _forms$,mat _mask$,mat _headings$;___,index_)
36660   data_file_nonempty=0
36680   mat _headings$(udim(_headings$)+1)
36700   mat _headings$(2:udim(_headings$))=_headings$(1:udim(_headings$)-1)
36720   _headings$(1)="Combined"
36740   mat _widths(udim(_headings$))=(0): mat _forms$(udim(_headings$))=('')
36760   ! 
36780   open #grid_data:=fngethandle: 'Name='&env$('temp')&'\'&trim$(path1$)&'[SESSION].tmp',display,input 
36800   for count=1 to 500
36820     linput #grid_data: _line$ eof ignore
36840     if file(grid_data)<>0 then goto GRIDFORM_COMPLETE
36860     str2mat(_line$,mat _chunks$,chr$(9))
36880     if udim(_chunks$)<> udim(_headings$)-1 then ! truncate extra columns, which are there by mistake or are missing
36900       mat _chunks$( udim(_headings$)-1 )
36920     end if 
36940     mat2str(mat _chunks$,_line$," ")
36960     _widths(1)=max(_widths(1),len(_line$)+udim(_chunks$)-1)
36980     for _index=1 to udim(mat _chunks$)
37000       _widths(_index+1)=max(_widths(_index+1),len(_chunks$(_index))+1)
37020       _widths(_index+1)=max(_widths(_index+1),len(_headings$(_index+1))+4)
37040       if _widths(_index+1) then data_file_nonempty=1
37060       _forms$(_index+1)="C "&str$(_widths(_index+1))&',L'
37080     next _index
37100   next count
37120   GRIDFORM_COMPLETE: ! 
37140   for index_=2 to udim(mat _mask$)+1
37160     fn_column_mask(_forms$(index_),_widths(index_),_mask$(index_-1))
37180   next index_
37200   _forms$(1)="0/C 500"
37220   _widths(1)=0
37240   close #grid_data: 
37260   fn_gridform=data_file_nonempty
37280 fnend 
37300 def fn_column_mask(&form$,&width,mask$;___,invisible)
37320   maxlen=width + 10 ! to deal with bad data
37340   mask=val(mask$) conv ignore
37360   if mask >= 1000 then 
37380     mask -= 1000
37400     invisible=1
37420   end if 
37440   if mask=1 then ! date format : mm/dd/yy
37460     form$=str$(width)&'/date(m/d/yy)' ! 
37480   else if mask=2 then !    date format : mm/dd/ccyy
37500     form$=str$(width)&'/date(m/d/ccyy)' ! $$$$$ GSB This code used to say FMT(99/99/99) which is only 6 digits long and appears wrong
37520   else if mask=03 then !    date format : ccyy/mm/dd
37540     form$=str$(width)&'/date(ccyy/m/d)' ! '/fmt(9999/99/99)'
37560   else if mask=04 then !    date format : dd/mm/ccyy
37580     form$=str$(width)&'/date(d/m/ccyy)'
37600   else if mask=05 then !    date format : dd/mm/yy
37620     form$=str$(width)&'/date(d/m/yy)'
37640   else if mask=10 then ! dollars, 2 decimals, commas
37660     form$=''
37680     for tm_char_index=1 to maxlen-4
37700       if mod(tm_char_index,4)=0 then 
37720         form$(0:0)=','
37740       else 
37760         form$(0:0)='-'
37780       end if 
37800     next tm_char_index
37820     if form$(1:1)=',' then form$(0:0)='-'
37840     form$(0:0)=str$(width)&"/#PIC("
37860     form$(inf:inf)=".--)"
37880   else if mask=20 then ! 0 decimals, commas
37900     form$=''
37920     for tm_char_index=1 to maxlen
37940       if mod(tm_char_index,4)=0 then 
37960         form$(0:0)=','
37980       else 
38000         form$(0:0)='-'
38020       end if 
38040     next tm_char_index
38060     if form$(1:1)=',' then form$(0:0)='-'
38080     form$(0:0)=str$(width)&"/#PIC("
38100     form$(inf:inf)=")"
38120   else if mask=30 then ! defaults 1 to 1
38140     form$=str$(width)&"/#PIC("&rpt$('-',maxlen)&")"
38160   else if mask=31 then ! defaults 1 to 1.0
38180     form$=str$(width)&"/#PIC("&rpt$('-',maxlen-2)&".-)"
38200   else if mask=32 then ! defaults 1 to 1.00
38220     form$=str$(width)&"/#PIC("&rpt$('-',maxlen-3)&".--)"
38240   else if mask=33 then ! defaults 1 to 1.000
38260     form$=str$(width)&"/#PIC("&rpt$('-',maxlen-4)&".---)"
38280   else if mask=34 then ! defaults 1 to 1.0000
38300     form$=str$(width)&"/#PIC("&rpt$('-',maxlen-5)&".----)"
38320   else if mask=35 then ! defaults 1 to 1.00000
38340     form$=str$(width)&"/#PIC("&rpt$('-',maxlen-6)&".-----)"
38360   else if mask=36 then ! defaults 1 to 1.000000
38380     form$=str$(width)&"/#PIC("&rpt$('-',maxlen-7)&".------)"
38400   else if mask=40 then ! defaults 1 to 0.1
38420     form$=str$(width)&"/#PIC("&rpt$('-',maxlen)&")"
38440   else if mask=41 then ! defaults 1 to 0.10
38460     form$=str$(width)&"/#PIC("&rpt$('-',maxlen-2)&".-)"
38480   else if mask=42 then ! defaults 1 to 0.100
38500     form$=str$(width)&"/#PIC("&rpt$('-',maxlen-3)&".--)"
38520   else if mask=43 then ! defaults 1 to 0.1000
38540     form$=str$(width)&"/#PIC("&rpt$('-',maxlen-4)&".---)"
38560   else if mask=44 then ! defaults 1 to 0.10000
38580     form$=str$(width)&"/#PIC("&rpt$('-',maxlen-5)&".----)"
38600   else if mask=45 then ! defaults 1 to 0.100000
38620     form$=str$(width)&"/#PIC("&rpt$('-',maxlen-6)&".-----)"
38640   else if mask=46 then ! defaults 1 to 0.1000000
38660     form$=str$(width)&"/#PIC("&rpt$('-',maxlen-7)&".------)"
38680   else if mask=50 then 
38700     form$=str$(width)&"/#PIC(------)" ! ######
38720   else if mask=51 then 
38740     form$=str$(width)&"/#PIC(---D------)" ! ###-######
38760   else if mask=52 then 
38780     form$=str$(width)&"/#PIC(------D---)" ! ######-###
38800   else if mask=53 then 
38820     form$=str$(width)&"/#PIC(---D------D---)" ! ###-######-###
38840   else if mask=51 then !        general ledger : ###-######
38860     form$='fmt(999-999999)'
38880   else if mask=52 then !        general ledger : ######-###
38900     form$='fmt(999999-999)'
38920   else if mask=53 then !        general ledger : ###-######-###
38940     form$='fmt(999-999999-999)'
38960   else if mask=61 then !        general ledger : ############
38980     form$='fmt(999999999999999)'
39000   else if mask=65 then !        12 decimals no commas
39020     form$='#PIC(------.------------)'
39040   else if mask=80 then !        force column left aligned
39060     form$="C "&str$(width)
39080   else if mask=81 then !        force column right aligned
39100     form$="CR "&str$(width)
39120   end if 
39140   if invisible then width=0
39160 fnend 
39720 def fn_print_grid
39721   ! library 'S:\Core\fnsnap\rtflib_dll.br': fnlistprint
39722   ! fnLISTPRINT(LISTWIN,LISTSPEC$,"Selected Clients","","Selected clients  for "&CLNR$,MAT DUMMY,0,0,"11111",0) : GOTO 5290
39723   fnlistprint(0,gridspec$,"","","Grid Print",mat dummy,0,0,"000000",0)
39724   !   goto PRINT_GRID_XIT
39740   !   fnopenprn
39750   !   mat2str(mat _headings$(2:udim(_headings$)),_line$,tab$)
39752   !   pr #255: _line$
39760   !   input fields gridspec$&",rowcnt,all,nowait": grid_rows
39780   !   input fields gridspec$&",colcnt,all,nowait": grid_columns
39800   !   mat _chunks$(grid_columns)
39820   !   for rowindex_=1 to grid_rows
39840   !     input fields gridspec$&",row,range,nowait": rowindex_, rowindex_, mat _chunks$
39860   !     mat2str(mat _chunks$,_line$," ")
39880   !     pr #255: _line$
39900   !   next rowindex_
39920   !   fncloseprn
39930   ! PRINT_GRID_XIT: !
39940 fnend 
39960 def fn_ace_rd_pic
39980   lyne=val(control$(2))
40000   ps=val(control$(3))
40020   width=val(control$(4))
40040   height=val(control$(5))
40060   path1$=control$(6)
40080 fnend 
40100 def fn_ace_rd_button
40120   lyne=val(control$(2))
40140   ps=val(control$(3))
40160   height=val(control$(4))
40180   width=val(control$(5))
40200   comkey=val(control$(6))
40220   ! 
40240   mat return_keys(udim(return_keys)+1)
40260   return_keys(udim(return_keys))=comkey
40280   ! 
40300   txt$=srep$(trim$(control$(7)),chr$(38),"")
40320   tt$=control$(8) error ignore
40340   default=val(control$(9)) error ignore
40360   cancel=val(control$(10)) error ignore
40380   container=val(control$(11)) error ignore
40400   tabcon=val(control$(12)) error ignore
40420   dim spec$*255
40422   spec$=str$(lyne)&','&str$(ps)&','&str$(width)&'/CC '&str$(len(txt$))&',,B'&str$(comkey)
40424   ! r: new logic 10/19/2015
40426   !   spec$=str$(lyne)&','&str$(ps)&',CC '&str$(width)&',,B'&str$(comkey)
40428   if default then 
40430     default_button_fkey=comkey
40432     spec$=str$(lyne)&','&str$(ps)&','&str$(width)&'/CC '&str$(len(txt$))&',[buttons],B'&str$(comkey)
40434     pr #0, fields "1,5,P 1/2,[buttons],"&str$(comkey): "S:\Core\Icon\forward-icon.png" ioerr ignore
40436   else if cancel then 
40438     spec$=str$(lyne)&','&str$(ps)&','&str$(width)&'/CC '&str$(len(txt$))&',[buttoncancel],B'&str$(comkey)
40440     if env$('tmp_acs_back_arrow')<>'' then 
40442       pr #0, fields "1,2,P 1/2,[buttons],"&str$(comkey): env$('tmp_acs_back_arrow') ioerr ignore
40444     else 
40446       pr #0, fields "1,2,P 1/2,[buttons],"&str$(comkey): "S:\Core\Icon\back-icon.png" ioerr ignore
40448     end if 
40450   else 
40452     spec$=str$(lyne)&','&str$(ps)&','&str$(width)&'/CC '&str$(len(txt$))&',,B'&str$(comkey)
40454   end if 
40456   ! /r
40460   if container then 
40480     tmp_win=frames(container,1) ! pr #frames(container,1), fields spec$: txt$
40500   else if tabcon then 
40520     tmp_win=tabs(tabcon,1) ! pr #tabs(tabcon,1), fields spec$: txt$
40540   else 
40560     tmp_win=acs_win ! pr #acs_win, fields spec$: txt$
40580   end if 
40582   ! r: new help logic 10/19/2015
40584   dim _help$*255
40586   if tt$='' then 
40588     _help$="1;Press ["
40590     if default then 
40592       _help$(inf:inf)="Enter"
40594     else if cancel then 
40596       _help$(inf:inf)="Esc"
40598     else if returnkey >=2 and returnkey<=12 then 
40600       _help$(inf:inf)="F"&str$(returnkey)
40602     else 
40604       _help$=''
40606     end if 
40608     if not _help$='' then _help$(inf:inf)="] to "&txt$&";"
40610   else 
40612     _help$='1;'&tt$&';'
40614   end if 
40616   ! /r
40618   pr #tmp_win, fields spec$, help _help$: txt$
40619 fnend 
40620 def fn_ace_rd_option
40640   respc+=1
40660   lyne=val(control$(2))
40680   ps=val(control$(3))
40700   align=val(control$(4))
40720   txt$=control$(5)
40740   container=val(control$(6))
40760   tabcon=val(control$(7))
40780   ! txt$=srep$(txt$,'"','""') ! fn2quote(txt$)
40800   if trim$(uprc$(resp$(respc)))<>"TRUE" and trim$(uprc$(resp$(respc)))<>"FALSE" then resp$(respc)="FALSE"
40820   txt$=ltrm$(txt$,'^')
40840   if trim$(uprc$(resp$(respc)))="TRUE" then resp$(respc)='^'&trim$(txt$) else resp$(respc)=trim$(txt$)
40860   if container then 
40880     fn_ace_io_add('#'&str$(frames(container,1))&','&str$(lyne)&','&str$(ps)&',radio '&str$(len(trim$(txt$))+4)&',T') ! tab order
40900   else if tabcon then 
40920     fn_ace_io_add('#'&str$(tabs(tabcon,1))&','&str$(lyne)&','&str$(ps)&',radio '&str$(len(trim$(txt$))+4)&',T') ! tab order
40940   else 
40960     fn_ace_io_add('#'&str$(acs_win)&','&str$(lyne)&','&str$(ps)&',radio '&str$(len(trim$(txt$))+4)&',T') ! tab order
40980   end if 
41000 fnend 
41020 def fn_ace_rd_check
41040   respc+=1
41060   lyne=val(control$(2))
41080   ps=val(control$(3))
41100   if ps<=0 then ps=1
41120   align=val(control$(4))
41140   txt$=control$(5)
41160   container=val(control$(6))
41180   tabcon=val(control$(7))
41190   chk_disable=val(control$(8))
41200   ! txt$=srep$(txt$,'"','""') ! fn2quote(txt$)
41220   txt$=ltrm$(txt$,'^')
41240   if trim$(uprc$(resp$(respc)))="TRUE" then resp$(respc)='^' else resp$(respc)=''
41260   spec$=','&str$(lyne)&','&str$(ps+len(txt$)+1)&',check 2'
41280   if align then 
41300     align$='CR'
41320   else 
41340     align$='C'
41360   end if 
41370   if chk_disable then chk_protected$=',P' else chk_protected$=',T' ! either Protect the field or force it to be in the tab order
41380   if container then 
41400     fn_ace_io_add('#'&str$(frames(container,1))&spec$&chk_protected$) ! Tab Order
41420     pr #frames(container,1), fields str$(lyne)&','&str$(ps)&','&align$&str$(len(txt$)): trim$(txt$)
41440   else if tabcon then 
41460     fn_ace_io_add('#'&str$(tabs(tabcon,1))&spec$&chk_protected$) ! Tab Order
41480     pr #tabs(tabcon,1), fields str$(lyne)&','&str$(ps)&','&align$&str$(len(txt$)): trim$(txt$)
41500   else 
41520     fn_ace_io_add('#'&str$(acs_win)&spec$&chk_protected$) ! Tab Order
41540     pr #acs_win, fields str$(lyne)&','&str$(ps)&','&align$&str$(len(txt$)): trim$(txt$)
41560   end if 
41580 fnend 
41600 def fn_ace_rd_label
41620   lyne=val(control$(2))
41640   ps=val(control$(3))
41660   mylen=val(control$(4))
41680   align=val(control$(5))
41700   txt$=control$(6)
41720   container=val(control$(7))
41740   tabcon=val(control$(8))
41760   font_mod=val(control$(9))
41780   txt$=srep$(trim$(txt$),'&','')
41790   dim lbl_tooltip$*256
41792   if udim(control$)>=10 then lbl_tooltip$=control$(10) else lbl_tooltip$=''
41800   if txt$<>'' then 
41820   ! txt$=srep$(txt$,'"','""') ! fn2quote(txt$)
41840     mylen=max(mylen,len(txt$))
41860     if align=1 then ace_rd_label_align$='r' else if align=2 then ace_rd_label_align$='c' else ace_rd_label_align$='l'
41880     if container then 
41890       lbl_win=frames(container,1)
41900     ! pr #frames(container,1), fields str$(lyne)&','&str$(ps)&',C'&ace_rd_label_align$&' '&str$(mylen): trim$(txt$)
41910     else if tabcon then 
41920       lbl_win=tabs(tabcon,1)
41930     ! pr #tabs(tabcon,1), fields str$(lyne)&','&str$(ps)&',C'&ace_rd_label_align$&' '&str$(mylen): trim$(txt$)
41940     else 
41950       lbl_win=acs_win
41960     ! pr #acs_win, fields str$(lyne)&','&str$(ps)&',C'&ace_rd_label_align$&' '&str$(mylen): trim$(txt$)
41970     end if 
41980     if lbl_tooltip$<>'' then 
41990       pr #lbl_win, fields str$(lyne)&','&str$(ps)&',C'&ace_rd_label_align$&' '&str$(mylen), help '4;'&lbl_tooltip$&';': trim$(txt$)
42000     else 
42010       pr #lbl_win, fields str$(lyne)&','&str$(ps)&',C'&ace_rd_label_align$&' '&str$(mylen): trim$(txt$)
42020     end if 
42030   end if 
42040 fnend 
42060 def fn_ace_rd_text
42080   respc+=1
42100   lyne=val(control$(2))
42120   ps=val(control$(3)) ! -.6
42140   width=val(control$(4))
42160   maxlen=max(val(control$(5)),width)
42180   if maxlen=1 then maxlen=2
42200   align=val(control$(6))
42220   disable=val(control$(7))
42240   mask$=control$(8)
42260   tt$=control$(9)
42280   container=val(control$(10))
42300   tabcon=val(control$(11))
42320   addtomask$=control$(12)
42340 ! resp$(respc)=srep$(resp$(respc),'"','""') soflow ignore ! fn2quote(resp$(respc))
42350 ! tt$=srep$(tt$,'"','""') soflow ignore ! fn2quote(tt$)
42360   dim spec$*255
42380   spec$=fn_textmask$(mask$,lyne,ps,width,container,maxlen)
42400   mat text_masks(respc)
42420   text_masks(respc)=val(mask$)
42440   if container then 
42460     fn_ace_io_add('#'&str$(frames(container,1))&','&str$(lyne)&','&str$(ps)&','&spec$)
42480   else if tabcon then 
42500     fn_ace_io_add('#'&str$(tabs(tabcon,1))&','&str$(lyne)&','&str$(ps)&','&spec$)
42520   else 
42540     fn_ace_io_add('#'&str$(acs_win)&','&str$(lyne)&','&str$(ps)&','&spec$)
42560   end if 
42580 fnend 
42600 def fn_textmask$*255 (mask$*255,lyne,ps,width,container,maxlen)
42620   dim attr$*255
42640   attr$=''
42660   ! 
42680   mask=0
42700   mask=val(mask$) conv MASK_VAL_CONV
42720   MASK_VAL_CONV: ! 
42960   ! 
42980   if mask>1000 then mask-=1000
43000   if mask>=1 and mask<=5 then 
43020     fn_datetextbox(mask,lyne,ps,width,container,disable)
43040     ! 
43060     attr$="9/#PIC(--/--/--)" ! attr$="9/DATE(m/d/y)"
43080     if mask=1 then 
43100       resp$(respc)=lpad$(trim$(resp$(respc)),6,'0')
43120       date_format$='mdy'
43140     else if mask=2 then 
43160       date_format$='mdcy'
43180       resp$(respc)=lpad$(trim$(resp$(respc)),8,'0')
43200     else if mask=3 then 
43220       date_format$='cymd'
43240       if len(trim$(resp$(respc)))<=6 then 
43260         resp$(respc)=date$(days(val(lpad$(trim$(resp$(respc)),6,'0')),'mdy'),'cymd')
43280         if val(resp$(respc)(1:2))=19 and val(resp$(respc)(3:4)) < 20 then resp$(respc)(1:2)="20"
43300       end if 
43320     else if mask=4 then 
43340       date_format$='dmcy'
43360       resp$(respc)=lpad$(trim$(resp$(respc)),8,'0')
43380     else if mask=5 then 
43400       date_format$='dmy'
43420       resp$(respc)=lpad$(trim$(resp$(respc)),6,'0')
43440     end if 
43460     ! 
43480     resp$(respc)=date$(days(trim$(resp$(respc)),date_format$),'mdy')
43500     ! 
43520   else if mask=9 then ! defaults 100 to 1.00
43540     attr$=str$(width)&"/#PIC("&rpt$('-',maxlen-3)&".--)"
43560     resp$(respc)=str$(val(resp$(respc))/100)
43580   else if mask=10 then ! dollars, 2 decimals, commas
43600     form$=''
43620     for tm_char_index=1 to maxlen-4
43640 !     if mod(tm_char_index,4)=0 then
43660 !       attr$(0:0)=','
43680 !     else 
43700       attr$(0:0)='-'
43720 !     end if
43740     next tm_char_index
43760     if maxlen < 4 then attr$(0:0)='-'
43780     attr$(0:0)=str$(width)&"/#PIC("
43800     attr$(inf:inf)=".--)"
43810     attr$=srep$(attr$,'/#PIC(,---.--','/#PIC(----.--')
43820   else if mask=12 then ! defaults 100 to 100.00, currency : American (2 decimals, commas)
43840     form$=''
43860     for tm_char_index=1 to maxlen-4
43880       if mod(tm_char_index,4)=0 then 
43900         attr$(0:0)=','
43920       else 
43940         attr$(0:0)='-'
43960       end if 
43980     next tm_char_index
43982 ! pr attr$ : pause
44000     attr$(0:0)=str$(width)&"/#PIC("
44020     attr$(inf:inf)=".--)"
44040   else if mask=20 then ! 0 decimals, commas
44060     form$=''
44080     for tm_char_index=1 to maxlen
44100       if mod(tm_char_index,4)=0 then 
44120         attr$(0:0)=','
44140       else 
44160         attr$(0:0)='-'
44180       end if 
44200     next tm_char_index
44220     attr$(0:0)=str$(width)&"/#PIC("
44240     attr$(inf:inf)=")"
44260   else if mask=30 then ! defaults 1 to 1
44280     attr$=str$(width)&"/#PIC("&rpt$('-',maxlen)&")"
44300   else if mask=31 then ! defaults 1 to 1.0
44320     attr$=str$(width)&"/#PIC("&rpt$('-',maxlen-2)&".-)"
44340   else if mask=32 then ! defaults 1 to 1.00
44360     attr$=str$(width)&"/#PIC("&rpt$('-',maxlen-3)&".--)"
44380   else if mask=33 then ! defaults 1 to 1.000
44400     attr$=str$(width)&"/#PIC("&rpt$('-',maxlen-4)&".---)"
44420   else if mask=34 then ! defaults 1 to 1.0000
44440     attr$=str$(width)&"/#PIC("&rpt$('-',maxlen-5)&".----)"
44460   else if mask=35 then ! defaults 1 to 1.00000
44480     attr$=str$(width)&"/#PIC("&rpt$('-',maxlen-6)&".-----)"
44500   else if mask=36 then ! defaults 1 to 1.000000
44520     attr$=str$(width)&"/#PIC("&rpt$('-',maxlen-7)&".------)"
44540   else if mask=40 then ! defaults 1 to 0.1
44560     attr$=str$(width)&"/#PIC("&rpt$('-',maxlen)&")"
44580 !   resp$(respc)=str$(val(resp$(respc))/10)
44600   else if mask=41 then ! defaults 1 to 0.10
44620     attr$=str$(width)&"/#PIC("&rpt$('-',maxlen-2)&".-)"
44640 !   resp$(respc)=str$(val(resp$(respc))/10)
44660   else if mask=42 then ! defaults 1 to 0.100
44680     attr$=str$(width)&"/#PIC("&rpt$('-',maxlen-3)&".--)"
44700 !   resp$(respc)=str$(val(resp$(respc))/10)
44720   else if mask=43 then ! defaults 1 to 0.1000
44740     attr$=str$(width)&"/#PIC("&rpt$('-',maxlen-4)&".---)"
44760 !   resp$(respc)=str$(val(resp$(respc))/10)
44780   else if mask=44 then ! defaults 1 to 0.10000
44800     attr$=str$(width)&"/#PIC("&rpt$('-',maxlen-5)&".----)"
44820 !   resp$(respc)=str$(val(resp$(respc))/10)
44840   else if mask=45 then ! defaults 1 to 0.100000
44860     attr$=str$(width)&"/#PIC("&rpt$('-',maxlen-6)&".-----)"
44880 !   resp$(respc)=str$(val(resp$(respc))/10)
44900   else if mask=46 then ! defaults 1 to 0.1000000
44920     attr$=str$(width)&"/#PIC("&rpt$('-',maxlen-7)&".------)"
44940 !   resp$(respc)=str$(val(resp$(respc))/10)
44960   else if mask=50 then 
44980     attr$=str$(width)&"/#PIC(------)" ! ######
45000   else if mask=51 then 
45020     attr$=str$(width)&"/#PIC(---D------)" ! ###-######
45040   else if mask=52 then 
45060     attr$=str$(width)&"/#PIC(------D---)" ! ######-###
45080   else if mask=53 then 
45100     attr$=str$(width)&"/#PIC(---D------D---)" ! ###-######-###
45120   else if mask=60 then 
45140     attr$=str$(width)&"/#PIC(---D---D----)" ! ###-###-#### or ####-###-####
45160   else if mask=70 or mask=71 or mask=72 then 
45180     fn_fileselection(mask,lyne,ps,width,container)
45200   end if 
45220   if attr$='' then 
45240     if align=1 then 
45260       attr$=str$(width)&'/CR '&str$(maxlen) ! right
45280     else if align=2 then 
45300       attr$=str$(width)&'/CC '&str$(maxlen) ! centered
45320     else 
45340       attr$=str$(width)&'/C '&str$(maxlen) ! left=default
45360     end if 
45380   end if 
45400 ! 
45420   if disable then protected$='P' else protected$='T' ! either Protect the field or force it to be in the tab order
45440   attr$(inf:inf)= ','&protected$&'[textboxes]' &','&str$(txtbox_fkey)
45450   attr$=srep$(attr$,'PIC(,','PIC(')
45460   fn_textmask$=attr$
45480 fnend 
45500 def fn_fileselection(mask,lyne,ps,width,container)
45520   dim file_select_button_spec$*255
45540   file_select_boxes+=1
45560   file_select_button_spec$=str$(lyne)&','&str$(ps+width+1)&',C 1,,B'&str$(file_select_fkey_base+file_select_boxes)
45580   if container then 
45600     file_select_button_spec$(0:0)='#'&str$(frames(container,1))&','
45620   else if tabcon then 
45640     file_select_button_spec$(0:0)='#'&str$(tabs(tabcon,1))&','
45660   else 
45680     file_select_button_spec$(0:0)='#'&str$(acs_win)&','
45700   end if 
45720   mat file_select_data(file_select_boxes,2)
45740   file_select_data(file_select_boxes,1)=respc
45760   file_select_data(file_select_boxes,2)=mask
45780   pr f file_select_button_spec$: "."
45800 fnend 
45820 def fn_selectfile (&filename$,mask)
45830   if mask=70 or mask=71 then 
45840     if env$('BR_MODEL')='CLIENT/SERVER' then 
45850       open #h_selectfile:=fngethandle: "Name=OPEN:@:"&filename$&"All documents (*.*) |*.*,RecL=1,Shr",external,input ioerr ignore
45860     else 
45870       open #h_selectfile:=fngethandle: "Name=OPEN:"&filename$&"All documents (*.*) |*.*,RecL=1,Shr",external,input ioerr ignore
45880     end if 
45890   else if mask=72 then 
45900     if env$('BR_MODEL')='CLIENT/SERVER' then 
45910       open #h_selectfile:=fngethandle: "Name=SAVE:@:"&filename$&"All documents (*.*) |*.*,RecL=1,new",external,output ioerr ignore
45920     else 
45930       open #h_selectfile:=fngethandle: "Name=SAVE:"&filename$&"All documents (*.*) |*.*,RecL=1,new",external,output ioerr ignore
45940     end if 
45950   else 
45960     goto SELECTFILE_COMPLETE
45970   end if 
45980   if file(h_selectfile)=0 then 
46000     filename$=os_filename$(file$(h_selectfile))
46001 ! filename$=trim$(file$(h_selectfile)) (2:inf)
46002     if filename$(1:2)='@:' then filename$(1:2)=''
46004     if filename$(1:1)=':' then filename$(1:1)=''
46020     close #h_selectfile: 
46040   end if 
46060 SELECTFILE_COMPLETE: ! 
46080 fnend 
46100 def fn_datetextbox (mask,lyne,ps,&width,container;disable)
46120   dim date_button_spec$*255
46140   date_button_spec$=''
46160   date_boxes+=1
46180   if disable then 
46200     date_button_spec$=str$(lyne)&','&str$(ps+width+1)&',P 1/2,P,'&str$(date_fkey_base+date_boxes)
46220   else 
46240     date_button_spec$=str$(lyne)&','&str$(ps+width+1)&',P 1/2,,'&str$(date_fkey_base+date_boxes)
46260   end if 
46280   mat date_fielddata(date_boxes,6)
46300   if container then 
46320     date_fielddata(date_boxes,1)=container
46340     date_fielddata(date_boxes,2)=lyne+frames(container,2)
46360     date_fielddata(date_boxes,3)=ps+frames(container,3)
46380     date_button_spec$(0:0)='#'&str$(frames(container,1))&','
46400   else if tabcon then 
46420     date_fielddata(date_boxes,1)=tabcon
46440     date_fielddata(date_boxes,2)=lyne+tabs(tabcon,2)
46460     date_fielddata(date_boxes,3)=ps+tabs(tabcon,3)
46480     date_button_spec$(0:0)='#'&str$(tabs(tabcon,1))&','
46500   else 
46520     date_fielddata(date_boxes,2)=lyne
46540     date_fielddata(date_boxes,3)=ps
46560     date_button_spec$(0:0)='#'&str$(acs_win)&','
46580   end if 
46600   pr f date_button_spec$: "S:\Core\Icon\calendar_icon.png" ioerr ignore
46620   date_fielddata(date_boxes,4)=width
46640   date_fielddata(date_boxes,5)=respc ! parent textbox number
46660   date_fielddata(date_boxes,6)=mask
46680 fnend 
48000 def fn_ace_rd_combo(combo$*1)
48020   dim ace_combo_io$*255
48040   ace_combo_io$=''
48060 ! combo_count=0
48080   respc+=1
48100   lyne=val(control$(2))
48120   ps=val(control$(3))
48140   width=val(control$(4))
48160 ! keylen=val(control$(5))
48180   path1$=control$(6)
48200   limittolist=val(control$(7))
48220   tt$=control$(8)
48240   container=val(control$(9))
48260   tabcon=val(control$(10))
48280   dim combooptionset$*256
48300   combooptionset$=control$(11)
48320 ! 
48340   tmp_combo_count_for_read+=1
48360 ! 
48380   if width<=2 then 
48400     width=4
48420   end if 
48422   if combo$='F' then 
48424     poskeylen=pos(combooptionset$,'lnk=')
48426     keylen=val(combooptionset$(poskeylen+4:pos(combooptionset$,',',poskeylen)-1))
48428     posdesclen=pos(combooptionset$,'lnd=')
48430     desclen=val(combooptionset$(posdesclen+4:pos(combooptionset$,',',posdesclen)-1))
48431     if desclen>0 then spacerlen=1 else spacerlen=0
48432     if (keylen+desclen+spacerlen)>width then 
48434       width=keylen+desclen+spacerlen
48436     end if 
48438   end if 
48440   if limittolist=1 or limittolist=2 then 
48460     ace_combo_io$=str$(lyne)&','&str$(ps)&','&str$(width)&'/combo 128,+,Select'
48480   else 
48500 !   ace_combo_io$=str$(lyne)&','&str$(ps)&','&str$(width)&'/combo 128,+'
48502     ace_combo_io$=str$(lyne)&','&str$(ps)&','&str$(width)&'/combo '&str$(width)&',+'
48520   end if 
48540 ! tt$=srep$(tt$,'"','""') ! fn2quote(tt$)
48560 ! r: test for invalid containter and/or tabcon
48580   if container>udim(mat frames,1) then 
48600     pr 'invalid container ('&str$(container)&') specified for combobox (type '&combo$&')'
48620     fnpause
48640     container=0
48660   end if 
48680   if tabcon>udim(mat tabs,1) then 
48700     pr 'invalid tab container (tabcon='&str$(container)&') specified for combobox (type '&combo$&')'
48720     fnpause
48740     tabcon=0
48760   end if 
48780 ! /r
48800   if container then 
48820     spec$='#'&str$(frames(container,1))&','&ace_combo_io$
48840   else if tabcon then 
48860     spec$='#'&str$(tabs(tabcon,1))&','&ace_combo_io$
48880   else 
48900     spec$='#'&str$(acs_win)&','&ace_combo_io$
48920   end if 
48940 ! 
48960 ! setenv('combo'&str$(respc),env$('tmp_combo'&str$(tmp_combo_count_for_read)))
48980   dim tmp_combo_option$(1)*81
49000   combooption_which=srch(mat combooptionsetlist$,combooptionset$)
49010 ! if comboOption_which=4 then pause
49020 ! if comboOption_which>comboOptionItemList(comboOption_which) then
49040 !   pr 'comboOptionSetList$(comboOption_which)=';comboOptionSetList$(comboOption_which)
49060 !   pr 'comboOptionItemList(comboOption_which)=';comboOptionItemList(comboOption_which)
49080 !   pr 'comboOption_which=';comboOption_which
49100 !   pr 'it is a repeat.'
49120 !   fnpause
49140 ! else 
49142 !   pr 'comboOption_which=';comboOption_which
49150 ! end if
49160 ! if comboOption_which>0 and comboOption_which<>comboOption_which_prior then
49180   str2mat(env$('tmp_combo'&str$(tmp_combo_count_for_read)),mat tmp_combo_option$,'|')
49184 ! else 
49186 !   pr 'saved time on tmp_combo'&str$(tmp_combo_count_for_read)
49200 ! end if
49202   combooption_which_prior=combooption_which
49220   pr f spec$: mat tmp_combo_option$
49240 ! 
49260   setenv('tmp_combo'&str$(tmp_combo_count_for_read)&'_response_item',str$(respc))
49280 ! COMBO_COMPLETE: !
49300 ! 
49320   if pos(spec$,"+,Select")>0 then 
49340     spec$=srep$(spec$,"+,Select","Select")
49360   else 
49380     spec$=srep$(spec$,",+",",")
49400   end if 
49420   spec$=spec$&'T[textboxes]'
49440   if pos (spec$,',Select')>0 then ! move the ,select to the end
49460     spec$=srep$(spec$,',Select',',')
49480     spec$=spec$&',Select'
49500   end if 
49520   fn_ace_io_add(spec$)
49540 fnend 
50000 def fn_ace_io_add(aia_in$*255)
50020   ace_io_count=udim(mat ace_io$)+1
50040   mat ace_io$(ace_io_count)
50060   ace_io$(ace_io_count)=aia_in$
50080 fnend 
51000 def fn_control_count
51020   control_count=val(env$('control_count'))+1
51040   setenv('control_count',str$(control_count))
51060   fn_control_count=control_count
51080 fnend 
54000 def library fnqgl(myline,mypos; qglcontainer,add_all_or_blank,use_or_replace,qgllength,qgltabcon)
54010   if ~setup then let fn_setup
54020   if qgllength=0 then qgllength=35
54030 ! _______________________________________________________________________
54040 ! the response$ for this - should be gotten with fnAGL
54050 ! fnCOMBOA("XXX",MYLINE,MYPOS,MAT OPT$,"Select from your Chart of Accounts ("&qgl_cursys$&").",WIDTH=35)
54060 ! this function has an integrated fnComboA - similar to the one above
54070 ! _______________________________________________________________________
54080   dim qglopt$*60
54090   dim glmstr_form$*80
54100   dim qgloption$(1)*255
54110   dim qgloptfile$*199
54120   dim qglsetupkeycurrent$*128
54130   dim qglsetupkeyprior$*128
54140 ! r: set qgl_cursys$ (for fnqgl purposes only)
54150   if env$('CurSys')="UB" and exists(env$('Q')&"\GLmstr\Company.h"&env$('cno')) then 
54160     qgl_cursys$="GL"
54170   else if env$('CurSys')="PR" then 
54180     if exists(env$('Q')&"\GLmstr\Company.h"&env$('cno')) then 
54190       qgl_cursys$="GL"
54200     else if exists(env$('Q')&"\CLmstr\Company.h"&env$('cno')) then 
54210       qgl_cursys$="CL"
54220     else 
54230       qgl_cursys$="PR"
54240     end if 
54250   else if env$('CurSys')='CR' and exists(env$('Q')&"\GLmstr\Company.h"&env$('cno')) then 
54260     qgl_cursys$='GL'
54270   else if env$('CurSys')='CR' and exists(env$('Q')&"\GLmstr\Company.h"&env$('cno'))=0 then 
54280     qgl_cursys$='CR'
54290   else if env$('CurSys')='CL' then 
54300     qgl_cursys$='CL'
54310   else 
54320     qgl_cursys$='GL'
54330   end if 
54340 ! /r
54350   if setupqgl$<>qgl_cursys$ then ! r:
54360     setupqgl$=qgl_cursys$
54370     open #company:=fngethandle: "Name="&env$('Q')&"\"&qgl_cursys$&"mstr\Company.h"&env$('cno')&",Shr",internal,input ioerr CLOSECOMPANY
54380     if qgl_cursys$='CL' then 
54390       read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub ! read it from checkbook
54400     else if qgl_cursys$="GL" then 
54410       read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub ! read it from General Ledger
54420     else if qgl_cursys$="PR" or qgl_cursys$="UB" or qgl_cursys$="CR" then 
54430       use_dept=1: use_sub=1 ! default both to yes if from pr chart of accounts
54440     end if 
54450 CLOSECOMPANY: ! 
54460     close #company: ioerr ignore
54470     if use_dept<>0 and use_sub<>0 then glmstr_form$="Form Pos 1,C 12"
54480     if use_dept =0 and use_sub<>0 then glmstr_form$="Form Pos 4,C 09"
54490     if use_dept =0 and use_sub =0 then glmstr_form$="Form Pos 4,C 06"
54500     if use_dept<>0 and use_sub =0 then glmstr_form$="Form Pos 1,C 09"
54510 ! add description to the form
54520     glmstr_form$=glmstr_form$&",Pos 13,C 50"
54530   end if  ! /r
54540   qglsetupkeycurrent$='qglCursys='&qgl_cursys$&',add_all_or_blank='&str$(add_all_or_blank)
54550   if qglsetupkeycurrent$=qglsetupkeyprior$ then 
54560 !   pr 'saving time ';timesavecount+=1 : pause
54570     goto QGLFINIS
54580   else 
54590     qglsetupkeyprior$=qglsetupkeycurrent$
54600     mat qgloption$(0) : option_count=0
54610     if add_all_or_blank=1 then 
54620       mat qgloption$(option_count+=1) : qgloption$(option_count)='[All]'
54630     else if add_all_or_blank=2 then 
54640       mat qgloption$(option_count+=1) : qgloption$(option_count)=''
54650     end if 
54660 ! read the chart of accounts from the appropriate system into an array
54670     if qgl_cursys$='GL' or qgl_cursys$='CL' or qgl_cursys$='PR' or qgl_cursys$='UB' or qgl_cursys$='CR' then 
54680       open #glmstr:=fngethandle: "Name="&env$('Q')&"\"&qgl_cursys$&"mstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\"&qgl_cursys$&"mstr\glIndex.h"&env$('cno')&",Shr",internal,input,keyed ioerr QGL_ERROR
54690     end if 
54700     do 
54710       read #glmstr,using glmstr_form$: qglopt$,desc$ norec QGL_LOOP_COMPLETE eof EO_QGL_GLMSTR ioerr QGL_ERROR
54720 ! reformat the options for typing
54730       if use_dept<>0 and use_sub<>0 then 
54740         qglopt$=trim$(qglopt$(1:3))&"-"&trim$(qglopt$(4:9))&"-"&trim$(qglopt$(10:12))
54750       else if use_dept=0 and use_sub<>0 then 
54760         qglopt$=trim$(qglopt$(1:6))&"-"&trim$(qglopt$(7:9))
54770       else if use_dept=0 and use_sub=0 then 
54780         qglopt$=trim$(qglopt$(1:6))
54790       else if use_dept<>0 and use_sub=0 then 
54800         qglopt$=trim$(qglopt$(1:3))&"-"&trim$(qglopt$(4:9))
54810       end if 
54820 !  add spaces to the end of it
54830 !  - for spacing of the description,
54840 !  and the description
54850       qglopt$=(rpad$(qglopt$,14)&desc$)(1:qgllength)
54860 !   write it into the comobobox option file
54870 !   pr #whr,using "Form Pos 1,C 81": qglOpt$
54880       mat qgloption$(option_count+=1) : qgloption$(option_count)=qglopt$
54890 QGL_LOOP_COMPLETE: ! 
54900     loop 
54910 EO_QGL_GLMSTR: ! 
54920     close #glmstr: ioerr ignore
54930   end if 
54940   goto QGLFINIS
54950 QGL_ERROR: ! 
54960   pr 'err ';err;' on line ';line
54970   pause 
54980   goto QGLFINIS
54990 QGLFINIS: ! WRITE_QGL_ACE: ! add it to the screen ace script file
55000   qgloptfile$=qgl_cursys$&"GLNumber"
55010   fn_comboa(qgloptfile$,myline,mypos,mat qgloption$, "Select from the Chart of Accounts ("&qgl_cursys$&").",qgllength,qglcontainer,qgltabcon,qglsetupkeycurrent$)
55020   myline=mypos=con=0
55030 fnend 
56000 def fn_remove_crlf(&txt$)
56020   lastx=x=0
56040   do 
56060     x=pos(txt$,hex$("0D0A"),lastx)
56080     if x>0 then 
56100       txt$=txt$(1:x-1)&"HEX$(0D0A)"&txt$(x+2:len(trim$(txt$)))
56120       lastx=x
56140     end if 
56160   loop while x>0
56180 fnend 
60000 def fn_export_grid(;___,index_)
60020   dim filename$*1000
60040   filename$=''
60060   grid_rows=grid_columns=index_=0
60080   open #export_file:=fngethandle: "Name=save:Text documents (*.txt) |*.txt,RecL=1,Replace",external,output error GRID_EXPORT_XIT
60100   filename$=file$(export_file)
60120   close #export_file: 
60140   open #export_file: 'Name='&filename$&',RecL=2048,Replace',display,output 
60160   input fields gridspec$&",RowCnt,all,nowait": grid_rows
60180   input fields gridspec$&",ColCnt,all,nowait": grid_columns
60200   mat _chunks$(grid_columns)
60220   mat2str(mat _headings$(2:udim(_headings$)),_line$,tab$)
60240   pr #export_file: _line$
60260   for index_=1 to grid_rows
60280     input fields gridspec$&",row,range,nowait": index_, index_, mat _chunks$
60300     for eg_grid_line_item=2 to udim(mat _chunks$)
60320       eg_tmp_mask=val(_mask$(eg_grid_line_item-1))
60340       if eg_tmp_mask=>1 and eg_tmp_mask<=5 then 
60360 ! r: get date_format$
60380         if eg_tmp_mask=1 then 
60400           date_format$='mdy'
60420         else if eg_tmp_mask=2 then 
60440           date_format$='mdcy'
60460         else if eg_tmp_mask=3 then 
60480           date_format$='cymd'
60500         else if eg_tmp_mask=4 then 
60520           date_format$='dmcy'
60540         else if eg_tmp_mask=5 then 
60560           date_format$='dmy'
60580         end if 
60600 ! /r
60620 !         pr _chunks$(eg_grid_line_item);'  date format is '&date_format$ : pause
60640         _chunks$(eg_grid_line_item)=date$(val(_chunks$(eg_grid_line_item)),date_format$)
60660       else if eg_tmp_mask<2 or eg_tmp_mask>65 then 
60680         _chunks$(eg_grid_line_item)='"'&rtrm$(_chunks$(eg_grid_line_item))&'"'
60700       end if 
60720     next eg_grid_line_item
60730 ! 
60740     mat2str(mat _chunks$(2:udim(_chunks$)),_line$,tab$)
60760     pr #export_file: _line$ ! pr _line$ : pause
60780   next index_
60800   close #export_file: 
60820 ! GRID_EXPORT_COMPLETE: !
60840   mat ml$(2)
60860   ml$(1)='Grid successfully exported to:'
60880   ml$(2)=os_filename$(filename$)
60900   fnmsgbox(mat ml$,resp$,"ACS",0)
60910 GRID_EXPORT_XIT: ! 
60920 fnend 
62000 def fn_get_flexhandle(;forceclose)
62020   if forceclose then 
62040     forceclose=0
62060     close #118: ioerr ignore
62080   end if 
62100   fn_get_flexhandle=118
62120 fnend 
64000 def library fnbackgrounddisable(; activate)
64020   if ~setup_library then let fn_setup
64040   fnbackgrounddisable=fn_backgrounddisable( activate)
64060 fnend 
64080 def fn_backgrounddisable(; activate)
64100   if activate then 
64120     acs_win_rows=val(env$('acs_win_rows')) : if acs_win_rows<=0 then acs_win_rows=35
64140     acs_win_cols=val(env$('acs_win_cols')) : if acs_win_cols<=0 then acs_win_cols=115
64160     open #disable_win=fngethandle: "srow=1,scol=1,rows="&str$(acs_win_rows)&",cols="&str$(acs_win_cols)&",border=none,picture=S:\Core\disable.png:TILE,parent=0",display,output 
64180   else 
64200     close #disable_win: ioerr ignore
64220   end if 
64240 fnend 
66000 def library fncompany_name(window,win_cols)
66020   if ~setup_library then let fn_setup
66040   fncompany_name=fn_company_name(window,win_cols)
66060 fnend 
68000 def fn_company_name(window,win_cols)
68160   pr #window, fields "1,08,CC 18,[screenheader]": date$("Month dd, ccyy")(1:18)
68180   pr #window, fields "1,27,CC 05,[screenheader]": env$("cno")
68200   pr #window, fields "1,33,CC 51,[screenheader]": env$('Program_Caption')(1:51)
68220   pr #window, fields "1,86,22/CC 24,[screenheader]": env$('cnam')(1:24)
68240   if env$('tmp_acs_back_arrow')='' then ! it is not the main menu.
68260     pr #window, fields "1,"&str$(win_cols-05)&",P 1/2,[buttons],1505": "S:\Core\Icon\Properties.png" ioerr ignore
68280   end if 
68300   pr #window, fields "1,"&str$(win_cols-02)&",P 1/2,[buttons],1504": "S:\Core\Icon\help_icon.png" ioerr ignore
68320 fnend 

