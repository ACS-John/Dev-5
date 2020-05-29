! replace S:\acsUB\calk_chatom
def library fncalkChatom(x$,d1,f,usage_water,x2,x3,mc1,mu1,mat rt,mat a,mat b,mat c,mat d,mat g,mat w,mat x,mat extra,mat gb,ratemst,unused,btu; calc_interest_on_deposit,charge_inspection_fee,interest_credit_rate)
	if ~setup_calk then ! r:
		setup_calk=1
		autoLibrary
		dim x$*10,gb(10),da(2),c(4),dp$*60,serviceName$(10)*20,tax_code$(10)*1,penalty$(10)*1,subjectto(10)
		dim san(3)
		san(1)=8 : san(2)=15 : san(3)=20
		FORM_RATEMSTR: form pos 55,32*g 10
		fnget_services(mat serviceName$,mat service$,mat tax_code$,mat penalty$,mat subjectto)
		for j=1 to udim(serviceName$)
			serviceName$(j)=trim$(serviceName$(j))
		next j
	end if  ! /r ~setup_calk then
	gosub WATER
	gosub SEWER
	gosub SANITATION
	gosub FIRE
	gosub OTHER_CHG
	gosub STAX
	gosub PENALTY
	gosub NET_GROSS
	goto CALK_EXIT
	WATER: ! r: calculate water
		if x(12)>0 then usage_water=x(12) ! water usage entered
		if x(9)><0 then w(1)=x(9) : goto WATER_END ! water charge entered
		if b(1)><0 then w(1)=b(1) : goto WATER_END ! standard water charge
		if d(13)<1 then d(13)=1
		read #ratemst,using FORM_RATEMSTR, key="WA"&lpad$(str$(a(1)),2): mc1,mu1,mat rt nokey WATER_END
		mu1=mu1*max(1,d(13))
		mc1=mc1*max(1,d(13))
		w(1)=mc1
		if usage_water>mu1 then w(1)=w(1)+(usage_water-mu1)*rt(1,3)
	WATER_END: g(1)=round(w(1),2)
		gosub WATER_USED
	return  ! /r
	SEWER: ! r: calculate sewer
	! pr x$ : if trim$(x$)='100106.00' then pause
		if x(5)><0 then w(2)=x(5) : goto SEWER_COMPLETED
		if b(2)><0 then w(2)=b(2) : goto SEWER_COMPLETED
		read #ratemst,using FORM_RATEMSTR, key="SW"&lpad$(str$(a(2)),2): mc1,mu1,mat rt nokey SEWER_COMPLETED
		mu1=mu1*max(1,d(13))
		mc1=mc1*max(1,d(13))
		! r: old chatom sewer calculation
		!   if usage_water=>mu1 then
		!     w(2)=mc1+(usage_water-mu1)*rt(a(2),3)
		!   else 
		!     w(2)=mc1
		!   end if
		! /r
		! r: new chatom sewer calculation - more standard... i hope
		usage_sewer=usage_water
		w(2)=mc1*max(1,extra(14)) ! units per meter - sewer (default to one)
		if usage_sewer<=mu1 then goto L3300 else mu2=mu1
		for j=1 to 10
			if rt(j,1)>usage_sewer then goto L3300
			if usage_sewer<rt(j,2) then w1=usage_sewer-mu2 else w1=rt(j,2)-mu2
			w(2)=w(2)+round(w1*rt(j,3),2)
			if rt(j,2)>usage_sewer then goto L3300
			mu2=rt(j,2)
		next j
	L3300: ! 
	! /r
	SEWER_COMPLETED: ! 
		w(2)=round(w(2),2)
		g(2)=w(2)
	return  ! /r
	ELECTRIC: ! r: NO ELECTRIC
		gosub ELECTRIC_USED
	return  ! /r
	GAS: ! r: NO GAS
		gosub GAS_USED
	return  ! /r
	SANITATION: ! r: Calculate sanitation
		if x(6)><0 then w(5)=x(6) : goto SANITATION_END
		if b(5)><0 then w(5)=b(5) : goto SANITATION_END
		read #ratemst,using FORM_RATEMSTR, key="SN"&lpad$(str$(a(5)),2): mc1,mu1,mat rt nokey SANITATION_END
		w(5)=rt(a(5),3)
		SANITATION_END: g(5)=w(5)
	return  ! /r
	FIRE: ! r: CALCULATE FIRE PROTECTION
		if x(7)><0 then g(6)=x(7) : goto FIRE_END
		if b(6)><0 then g(6)=b(6) : goto FIRE_END
		FIRE_END: !
	return  ! /r
	OTHER_CHG: ! r: CALCULATE OTHER CHARGES
		if x(8)><0 then g(8)=x(8) : goto OTHER_END
		if b(7)><0 then g(8)=b(7) : goto OTHER_END
	OTHER_END: return  ! /r
	STAX: ! r: CALCULATE SALES TAX
		if a(6)=9 then goto STAX_END
		read #ratemst,using FORM_RATEMSTR, key="TX"&lpad$(str$(a(6)),2): mc1,mu1,mat rt nokey STAX_END
		g(9)=round(g(1)*rt(a(6),3),2)
	STAX_END: !
	return  ! /r
	PENALTY: ! r:
		if a(7)=9 then goto PENALTY_END ! CALCULATE PENALTY
		a(7)=1 ! defalt to code 1
		read #ratemst,using FORM_RATEMSTR, key="PN"&lpad$(str$(a(7)),2): mc1,mu1,mat rt nokey PENALTY_END
		g(10)=mc1 ! standard penalty
	PENALTY_END: !
	return  ! /r
	NET_GROSS: ! r: calculate net & gross bill
		for j=1 to 10
			if uprc$(penalty$(j))><"Y" then g(11)=g(11)+g(j) ! don't add penalties into net
			g(12)=g(12)+g(j)
		next j
	return  ! /r
	WATER_USED: ! r: updates water readings & usages
		if d1><f then d(2)=d(1)
		d(1)=x(1)
		w8=d(3)
		d(3)=usage_water
		d(4)=d(4)+d(3)
		if d1=f then d(4)=d(4)-w8
	return  ! /r
	ELECTRIC_USED: ! r: updates electric readings & usages
		if d1><f then d(6)=d(5)
		d(5)=x(3)
		w8=d(7)
		d(7)=x2
		d(8)=d(8)+d(7)
		if d1=f then d(8)=d(8)-w8
	return  ! /r
	GAS_USED: ! r: updates gas readings & usages
		if d1><f then d(10)=d(9)
		d(9)=x(2)
		w8=d(11)
		d(11)=x3
		d(12)=d(12)+d(11)
		if d1=f then d(12)=d(12)-w8
	return  ! /r
	CALK_EXIT: ! 
fnend 
