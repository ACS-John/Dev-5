10000   def library fnmsgbox(&mat mg$; &response$,cap$*128,mt)
10020     library 'S:\Core\Library': fnerror,fntos,fnlbl,fncmdkey,fnacs
10040     on error goto ERTN
10060     if env$('exitnow')='yes' then let setenv('exitnow','cancelled by fnmsgbox')
10080     if cap$='' then let cap$=env$('Program_Caption')
10100 !   if env$('ACSDeveloper')<>'' then
10120     let fn_br_messagebox(mat mg$,response$, cap$,mt)
10140 !   else 
10160 !     fn_ace_messagebox(mat mg$,response$, cap$,mt)
10180 !   end if
10200 XIT: ! 
10220   fnend 
20000 ! <updateable region: ertn>
20020 ERTN: let fnerror(program$,err,line,act$,"xit")
20040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
20060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
20080   print "program pause: type go and press [enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
20100 ERTN_EXEC_ACT: execute act$ : goto ERTN
20120 ! </updateable region: ertn>
20140 IGNORE: continue 
30000   def fn_br_messagebox(&mat mg$, &response$; cap$*128, mt)
30020     dim bm_text$*2048
30030     let bm_text$=''
30040     let bm_type=mt
30050 ! 
30060     for mg_item=1 to udim(mat mg$)
30070       let bm_text$(inf:inf)=mg$(mg_item)&chr$(13)
30080     next mg_item
30100     let bm_text$=rtrm$(bm_text$,chr$(13))
30120 ! 
30130     let bm_btn_default=1
30160     if bm_type=>768 then let bm_type-=768 : let bm_btn_default=4
30180     if bm_type=>512 then let bm_type-=512 : let bm_btn_default=3
30200     if bm_type=>256 then let bm_type-=256 : let bm_btn_default=2
30210 ! 
30220     let bm_icon$=''
30240     if bm_type=>064 then let bm_type-=064 : let bm_icon$='Inf'
30260     if bm_type=>048 then let bm_type-=048 : let bm_icon$='Excl'
30280     if bm_type=>032 then let bm_type-=032 : let bm_icon$='Qst'
30300     if bm_type=>016 then let bm_type-=016 : let bm_icon$='Err'
31000 ! 
31020     if bm_type=5 then ! retry/cancel
31040       let fn_ace_messagebox(mat mg$,response$, cap$,mt)
31060       goto BM_XIT
31080     else if bm_type=4 then ! yes/no
31100       let bm_button$='yn'
31120     else if bm_type=3 then ! yes/no/cancel
31140       let bm_button$='ync'
31160     else if bm_type=2 then ! abort/retry/ignore
31180       let fn_ace_messagebox(mat mg$,response$, cap$,mt)
31200       goto BM_XIT
31220     else if bm_type=1 then ! ok/cancel
31240       let bm_button$='okc'
31260     else if bm_type=0 then ! ok
31280       let bm_button$='ok'
31300     end if 
31320     if bm_btn_default=2 and bm_type=1 then 
31340       let bm_button$='okC'
31360     else if bm_btn_default>1 and bm_type>0 then 
31380       let bm_button$(bm_btn_default:bm_btn_default)=uprc$(bm_button$(bm_btn_default:bm_btn_default))
31400     end if 
32000 ! 
32020 BM_ASK: ! 
32040     if bm_icon$='' then 
32060       let bm_response=msgbox(bm_text$,cap$,bm_button$)
32080     else 
32100       let bm_response=msgbox(bm_text$,cap$,bm_button$,bm_icon$)
32120     end if 
32140 ! 
32160     if bm_response=0 then 
32180       print 'msgbox returned an error'
32200       print 'please CALL SUPPORT or type GO and press Enter (Cancel will attempt to be selected)'
32220       pause 
32240       let bm_response=4
32260     end if 
32280     if bm_response=1 then let response$="OK"
32300     if bm_response=2 then let response$="Yes"
32320     if bm_response=3 then let response$="No"
32340     if bm_response=4 then let response$="Cancel"
32360     if response$="Cancel" and bm_type<>5 and bm_type<>3 and bm_type<>1 then ! only allow cancel on messageboxes that have a cancel option
32380       if bm_type=0 then ! If there was only an OK button, just assume they meant that
32400         let response$="OK"
32420       else 
32440         goto BM_ASK
32460       end if 
32480     end if 
32500 BM_XIT: ! 
32520   fnend 
60000   def fn_ace_messagebox(&mat mg$, &response$; cap$*128, mt)
60020     let mat_mg_len=0 : for j=1 to udim(mat mg$) : let mat_mg_len+=len(mg$(j)) : next j
60040     let fntos(sn$="mb"&str$(udim(mat mg$))&'-'&str$(mat_mg_len))
60060     for mg_item=1 to udim(mat mg$)
60080       let fnlbl(mg_item,1,mg$(mg_item))
60100     next mg_item
60120     mat btn_default=(0)
60140     if mt=>768 then let mt-=768 : let btn_default(4)=1
60160     if mt=>512 then let mt-=512 : let btn_default(3)=1
60180     if mt=>256 then let mt-=256 : let btn_default(2)=1
60200 ! if sum(mat btn_default)=0 then let btn_default(1)=1
60220     if mt=>064 then let mt-=064
60240     if mt=>048 then let mt-=048 ! 
60260     if mt=>032 then let mt-=032 ! 
60280     if mt=>016 then let mt-=016 ! (X) Critical
60300     if mt=5 then ! retry/cancel
60320       let fncmdkey("&Retry",4)
60340       let fncmdkey("&Cancel",99,btn_default(2),1)
60360     else if mt=4 then ! yes/no
60380       let fncmdkey("&Yes",6)
60400       let fncmdkey("&No",7,btn_default(2))
60420     else if mt=3 then ! yes/no/cancel
60440       let fncmdkey("&Yes",6)
60460       let fncmdkey("&No",7,btn_default(2))
60480       let fncmdkey("&Cancel",99,btn_default(3),1)
60500     else if mt=2 then ! abort/retry/ignore
60520       let fncmdkey("&Abort",3)
60540       let fncmdkey("&Retry",4,btn_default(2))
60560       let fncmdkey("&Ignore",5,btn_default(3))
60580     else if mt=1 then ! ok/cancel
60600       let fncmdkey("&Ok",1)
60620       let fncmdkey("&Cancel",99,btn_default(2),1)
60640     else if mt=0 then ! ok
60660       let fncmdkey("&Ok",1,1,1)
60680     end if 
60700     let fnacs(sn$,0,mat resp$,ckey,0,0,1,1)
60720     if ckey=1 then let response$="OK"
60740     if ckey=99 then let response$="Cancel"
60760     if ckey=3 then let response$="Abort"
60780     if ckey=4 then let response$="Retry"
60800     if ckey=5 then let response$="Ignore"
60820     if ckey=6 then let response$="Yes"
60840     if ckey=7 then let response$="No"
60900   fnend 
