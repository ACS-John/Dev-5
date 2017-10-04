10040   library 'S:\Core\Library': fntop,fnxit,fnacs_version$,fnclient_has_mat,fnSystemName$,fnmsgbox,fnacs,fnlbl,fntxt,fngethandle,fntos,fnerror,fncno,fncmdset,fnchk,fncd,fnactpd,fnstatus,fnqgl,fnagl$,fnindex_it,fnrgl$,fnclient_support
10060   on error goto ERTN
10100   dim txt$(1)*256
10140   fntop(program$, cap$="About ACS")
10160   c_has_count=fnclient_has_mat(mat c_has$)
10180   fnclient_support(mat system_id$,mat system_support_end_date,mat on_support)
20000   let txt_item=0
20020   mat txt$(txt_item+=1) : let txt$(txt_item)='ACS Version '&fnacs_version$
20040   mat txt$(txt_item+=1) : let txt$(txt_item)=chr$(9)&'Customized for '&env$('Client')
20060   if env$('user_limit')='1' then 
20080     mat txt$(txt_item+=1) : let txt$(txt_item)=chr$(9)&env$('user_limit')&' User Liscensed'
20100   else 
20120     mat txt$(txt_item+=1) : let txt$(txt_item)=chr$(9)&env$('user_limit')&' Users Liscensed'
20140   end if 
20160   mat txt$(txt_item+=1) : let txt$(txt_item)='Licensed Systems:'
20180   for c_has_item=1 to c_has_count
20200     mat txt$(txt_item+=1) : let txt$(txt_item)
20220     let txt$(txt_item)=chr$(9)&rpad$(fnSystemName$(c_has$(c_has_item)),40)
22000 ! r: add support information
22020     let which=srch(mat system_id$,c_has$(c_has_item))
22030     mat txt$(txt_item+=1) : let txt$(txt_item)
22040     if which>0 then 
22080       if days(date('ccyymmdd'),'ccyymmdd')<=days(system_support_end_date(which),'ccyymmdd') then 
22100         let txt$(txt_item)=chr$(9)&chr$(9)&'Support active until '
22120       else 
22140         let txt$(txt_item)=chr$(9)&chr$(9)&'Support expired on '
22160       end if 
22180       let txt$(txt_item)(inf:inf)=cnvrt$('pic(####/##/##)',system_support_end_date(which))
22220     else 
22240       let txt$(txt_item)(inf:inf)=chr$(9)&chr$(9)&'(no support data)'
22260     end if 
22280 ! /r
24000   next c_has_item
24020   mat txt$(txt_item+=1) : let txt$(txt_item)=''
24040   mat txt$(txt_item+=1) : let txt$(txt_item)='Business Rules! Version '&wbversion$
24060   mat txt$(txt_item+=1) : let txt$(txt_item)=chr$(9)&'Serial Number '&str$(serial)
24080   mat txt$(txt_item+=1) : let txt$(txt_item)=''
24100   mat txt$(txt_item+=1) : let txt$(txt_item)='For support contact ACS at:'
24120   mat txt$(txt_item+=1) : let txt$(txt_item)=chr$(9)&'1-800-643-6318'
24140   mat txt$(txt_item+=1) : let txt$(txt_item)=chr$(9)&'acs.bowman@gmail.com'
24160   mat txt$(txt_item+=1) : let txt$(txt_item)=chr$(9)&'planetacs.net'
24180   mat txt$(txt_item+=1) : let txt$(txt_item)=''
24200   mat txt$(txt_item+=1) : let txt$(txt_item)=chr$(9)&'Advanced Computer Services LLC'
24220   mat txt$(txt_item+=1) : let txt$(txt_item)=chr$(9)&'4 Syme Ave'
24240   mat txt$(txt_item+=1) : let txt$(txt_item)=chr$(9)&'West Orange, NJ 07052'
30000   fnmsgbox(mat txt$, response$, cap$)
30020   goto XIT
40000 ! <Updateable Region: ERTN>
40020 ERTN: let fnerror(program$,err,line,act$,"xit")
40040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
40060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
40080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
40100 ERTN_EXEC_ACT: execute act$ : goto ERTN
40120 ! /region
40140 XIT: let fnxit
