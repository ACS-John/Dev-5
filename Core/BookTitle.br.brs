24000   def library fnbooktitle$*256(x$*256)
24020     fnbooktitle$=fn_booktitle$(x$)
24040   fnend 
24060   def fn_booktitle$*256(x$*256)
24080     x$=lwrc$(trim$(x$)) : olda=0
24100     x$(1:1)=uprc$(x$(1:1))
24120 ! capitalize anthing after a SPACE
24140 L1240: ! 
24160     a=pos(x$," ",olda)
24180     if a<>0 then 
24200       a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a
24220       goto L1240
24240     end if 
24260     a=olda=0
24280 L1260: ! 
24300     a=pos(x$,"-",olda)
24320     if a<>0 then 
24340       a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a
24360       goto L1260
24380     end if 
24400     a=olda=0
24420 L1280: ! 
24440     a=pos(x$,"/",olda)
24460     if a<>0 then 
24480       a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a
24500       goto L1280
24520     end if 
24540     a=olda=0
24560 L1300: ! 
24580     a=pos(x$,"\",olda)
24600     if a<>0 then 
24620       a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a
24640       goto L1300
24660     end if 
24680     a=olda=0
24700 L1320: ! 
24720     a=pos(x$,".",olda)
24740     if a<>0 then 
24760       a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a
24780       goto L1320
24800     end if 
24820     fn_booktitle$=x$
24840   fnend 
