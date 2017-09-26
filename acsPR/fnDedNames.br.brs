0020 library program$: fnDedNames
0040 dim xa$(20)*20,xb$(20)*8,xc(20)
0060 let fnDedNames(mat xa$,mat xb$) ! , mat xc) ! ,mat xd,mat xe,mat xf,mat xg,mat xh,mat xi$)
0080 pr 'fullname','abbrName','dedcode'
0100 for x=1 to 20
0120   pr xa$(x),xb$(x),xc(x)
0140 nex x
0160 pause
0180 end
2000 def library fnDedNames(mat fullname$; mat abrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat gl$,doWrite)
2020   RESTART: !
2040   if ~setupDedNames then
2060     setupDedNames=1
2080     library 'S:\Core\Library': fngethandle
2100     dim cache_fullname$(20)*20
2120     dim cache_abrevname$(20)*8
2140     dim cache_dedcode(20)
2160     dim cache_calcode(20)
2180     dim cache_dedfed(20)
2200     dim cache_dedfica(20)
2220     dim cache_dedst(20)
2240     dim cache_deduc(20)
2260     dim cache_gl$(20)
2280     !
2300     mat cache_fullname$  =('')
2320     mat cache_abrevname$ =('')
2340     mat cache_dedcode    =(0)
2360     mat cache_calcode    =(0)
2380     mat cache_dedfed     =(0)
2400     mat cache_dedfica    =(0)
2420     mat cache_dedst      =(0)
2440     mat cache_deduc      =(0)
2460     mat cache_deduc      =(0)
2480     mat cache_gl$        =('')
2500     !
2520     if exists(env$('Q')&"\PRmstr\dednames.h"&env$('cno')) then 
2540       open #hdednames:=fngethandle: "Name="&env$('Q')&"\PRmstr\DedNames.h"&env$('cno'),internal,input,relative 
2560       read #hdednames,using fDedNames,rec=1: mat cache_fullname$,mat cache_abrevname$,mat cache_dedcode,mat cache_calcode,mat cache_dedfed,mat cache_dedfica,mat cache_dedst,mat cache_deduc,mat cache_gl$
2580       fDedNames: Form POS 1,20*C 20,20*C 8,120*N 1,20*C 12
2600     else
2620       open #hdednames:=fngethandle: "Name="&env$('Q')&"\PRmstr\dednames.h"&env$('cno')&",RecL=920,use",internal,outin,relative 
2640       write #hdednames,using fDedNames: mat cache_fullname$,mat cache_abrevname$,mat cache_dedcode,mat cache_calcode,mat cache_dedfed,mat cache_dedfica,mat cache_dedst,mat cache_deduc,mat cache_gl$
2660     end if 
2680     close #hdednames:
2700   end if
2720   !
2740   if doWrite then
2760     open #hdednames:=fngethandle: "Name="&env$('Q')&"\PRmstr\dednames.h"&env$('cno')&",RecL=920,use",internal,outin,relative
2780     rewrite #hdednames,using fDedNames,rec=1: mat fullname$, mat abrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat gl$
2800     close #hdednames:
2810     doWrite=0
2820     goto RESTART
2840   end if
2860   on error goto dnFINIS
2880   !
2900   mat fullname$(udim(mat cache_fullname$))
2920   mat fullname$=cache_fullname$
2940   mat abrevname$(udim(mat cache_abrevname$))
2960   mat abrevname$=cache_abrevname$
2980   mat dedcode(udim(mat cache_dedcode))
3000   mat dedcode=cache_dedcode
3020   mat calcode(udim(mat cache_calcode))
3040   mat calcode=cache_calcode
3060   mat dedfed(udim(mat cache_dedfed))
3080   mat dedfed=cache_dedfed
3100   mat dedfica(udim(mat cache_dedfica))
3120   mat dedfica=cache_dedfica
3140   mat dedst(udim(mat cache_dedst))
3160   mat dedst=cache_dedst
3180   mat deduc(udim(mat cache_deduc))
3200   mat deduc=cache_deduc
3220   mat gl$(udim(mat cache_gl$))
3240   mat gl$=cache_gl$
3260   dnFINIS: !
3280 fnend