18000 ! r: testing zone
18020 !    fn_setup
18040 !   fntop('W-3 Line Testing')
18060 !   fnpa_open('','W-3','PDF')
18080 !   fnpa_background('S:\Core\pdf\W-3.pdf')
18100 !   for lyne=1 to 14
18120 !     fnpa_txt(str$(lyne)&'. XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX' ,8,fn_line(lyne))
18140 !   nex lyne
18160 !   fnpa_finis
18180 !   pr 'done.' : end
19000 ! /r
20000 def fn_setup
20020   if ~setup then
20040     setup=1
20060     library 'S:\Core\Library': fntop,fnxit,fnchain,fnTos,fnFra,fnLbl,fnTxt,fnCmdKey,fnAcs,fnOpt,fnmsgbox,fnChk,fncmbemp,fnpa_finis,fnerror,fnureg_read,fnureg_write,fnButton,fnCmdSet,fnpa_open,fnpa_newpage,fnpa_fontsize,fnpa_txt,fncreg_read,fncreg_write,fnpa_background,fngethandle,fnDedNames,fnreg_read,fnreg_write,fncomboa
20080     fnreg_read('W-3 - Enable Background'              ,enableBackground$   ,'True' )
20160   end if
20180 fnend
30000 def library fnw3(taxyear$,ein$*12,mat a$,mat w,dcb,state$,stcode$)
30020   ! ein$      Employer Identification Number (EIN)
30040   ! a$(1)     Employer Name
30060   ! a$(2)     Employer Address
30080   ! a$(3)     Employer City, State, Zip
30100   ! w(1)      fed wh
30120   ! w(2)      taxable wages
30140   ! w(3)      ss wh
30160   ! w(4)      eic
30180   ! w(5)      ss wages
30200   ! w(6)      ss-tips
30220   ! w(7)      state wh
30240   ! w(8)      local wh
30260   ! w(9)      state wages
30280   ! w(10)     local wages
30300   ! w(11)     mc wages
30320   ! w(12)     mc medicare
30340   ! dcb       deferred comp
30360   ! state$    
30380   ! stcode$   
32000   if ~setup then let fn_setup
32020   fnpa_open('','W-3','PDF')
32040   if enableBackground$='True' then let fnpa_background('S:\Core\pdf\'&taxyear$&'\W-3.pdf')
32080   fnpa_fontsize(12)
32100   fnpa_txt("X",38,fn_line(2))
32120   col1=  8
32140   col2=113
32160   col3=169
32180   fnpa_txt(ein$,col1,fn_line(4)) ! Employer Identification Number (EIN)
32200   fnpa_txt(a$(1),col1,fn_line(6))
32220   fnpa_txt(a$(2),col1,fn_line(7)-2)
32240   fnpa_txt(a$(3),col1,fn_line(7)+2)
32260   fnpa_txt(cnvrt$("pic(--,---,---.##",w(2) ),col2,fn_line(4)) ! taxable wages
32280   fnpa_txt(cnvrt$("pic(--,---,---.##",w(1) ),col3,fn_line(4)) ! fed wh
32300   fnpa_txt(cnvrt$("pic(--,---,---.##",w(5) ),col2,fn_line(5)) ! ss wages
32320   fnpa_txt(cnvrt$("pic(--,---,---.##",w(3) ),col3,fn_line(5)) ! ss wh
32340   fnpa_txt(cnvrt$("pic(--,---,---.##",w(11)),col2,fn_line(6)) ! mc wages
32360   fnpa_txt(cnvrt$("pic(--,---,---.##",w(12)),col3,fn_line(6)) ! medicare wh
32380   fnpa_txt(cnvrt$("pic(--,---,---.##",w(6) ),col2,fn_line(7)) ! ss-tips
32400   fnpa_txt(cnvrt$("pic(--,---,---.##",w(4) ),col3,fn_line(8)) ! eic
34000   fnpa_txt(cnvrt$("pic(--,---,---.##",dcb  ),col3,fn_line(9)) ! deferred comp
34020   fnpa_txt(state$,12,fn_line(11))
34040   fnpa_txt(stcode$,22,fn_line(11))
34060   col1=  8
34080   col2= 51
34100   col3=110
34120   col4=169 
34140   fnpa_txt(cnvrt$("pic(--,---,---,zzz.##",w(9)),col1,fn_line(12)) ! state wages
34160   fnpa_txt(cnvrt$("pic(--,---,---,zzz.##",w(7)),col2,fn_line(12)) ! state wh
34180   fnpa_txt(cnvrt$("pic(--,---,---.##",w(10)),col3,fn_line(12)) ! local wages
34200   fnpa_txt(cnvrt$("pic(--,---,---.##",w(8)),col4,fn_line(12)) ! local wh
34220   fnpa_finis
34240 fnend
42000 def fn_line(lineNumber)
42020   lReturn=0
42060   if lineNumber=1 then 
42080     lReturn=10
42140   else  ! if lineNumber>=1 and lineNumber<=14 then
42160     lReturn=20+(8.5*(lineNumber-2))
42180   ! else 
42200   !   pr 'invalid lineNumber ('&str$(lineNumber)&')'
42220   !   pause
42240   end if 
42260   fn_line=lReturn
42280 fnend


