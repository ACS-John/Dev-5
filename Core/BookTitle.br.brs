	def library fnbooktitle$*256(x$*256)
		fnbooktitle$=fn_booktitle$(x$)
	fnend 
	def fn_booktitle$*256(x$*256)
		x$=lwrc$(trim$(x$)) : olda=0
		x$(1:1)=uprc$(x$(1:1))
! capitalize anthing after a SPACE
L1240: ! 
		a=pos(x$," ",olda)
		if a<>0 then 
			a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a
			goto L1240
		end if 
		a=olda=0
L1260: ! 
		a=pos(x$,"-",olda)
		if a<>0 then 
			a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a
			goto L1260
		end if 
		a=olda=0
L1280: ! 
		a=pos(x$,"/",olda)
		if a<>0 then 
			a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a
			goto L1280
		end if 
		a=olda=0
L1300: ! 
		a=pos(x$,"\",olda)
		if a<>0 then 
			a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a
			goto L1300
		end if 
		a=olda=0
L1320: ! 
		a=pos(x$,".",olda)
		if a<>0 then 
			a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a
			goto L1320
		end if 
		fn_booktitle$=x$
	fnend 
