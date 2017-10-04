00010 ! Replace S:\acsUB\CmbRt2.br
00020 ! create a screen ace combo box for available Route Numbers
20000   def library fncmbrt2(myline,mypos;all)
20020 ! all (0=place word all on list, 1=dont show an option for all)
20040     library 'S:\Core\Library': fnerror,fncomboa,fncreg_read
20080     dim option$(999)
20120     fncreg_read('Route Low',bkno1$) : bkno1=val(bkno1$)
20140     fncreg_read('Route High',bkno2$) : bkno2=val(bkno2$)
20180     if all=1 then 
20200       let x=1
20220     else 
20240       let option$(1)="[All]"
20260       let x=2
20280     end if 
20300     for j=bkno1 to bkno2
20320       let option$(x)=str$(j)
20340       let x+=1
20360     next j
20380     mat option$(x-1)
20400     fncomboa("CmbRt2",myline,mypos,mat option$,"If the Route Number you want is not listed you can change your route number range in Configure > Company",5)
20420   fnend 
