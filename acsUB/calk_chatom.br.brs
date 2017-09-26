10000 ! replace S:\acsUB\calk_chatom
10400   def library fncalk(x$,d1,f,usage_water,x2,x3,mc1,mu1,mat rt,mat a,mat b,mat c,mat d,mat g,mat w,mat x,mat extra,mat gb,ratemst,deposit2,btu; calc_interest_on_deposit,charge_inspection_fee,interest_credit_rate)
10500     if ~setup_calk then ! r:
10600       let setup_calk=1
10700       library 'S:\Core\Library': fnget_services,fnpause
10800       dim x$*10,gb(10),da(2),c(4),dp$*60,servicename$(10)*20,tax_code$(10)*1,penalty$(10)*1,subjectto(10)
11000       dim san(3)
11100       let san(1)=8 : let san(2)=15 : let san(3)=20
11400 FORM_RATEMSTR: form pos 55,32*g 10
11500       fnget_services(mat servicename$,mat service$,mat tax_code$,mat penalty$,mat subjectto)
11800       for j=1 to udim(servicename$)
11900         let servicename$(j)=trim$(servicename$(j))
12000       next j
12100     end if  ! /r ~setup_calk then
12200     gosub WATER
12300     gosub SEWER
12400     gosub SANITATION
12500     gosub FIRE
12600     gosub OTHER_CHG
12700     gosub STAX
12800     gosub PENALTY
12900     gosub NET_GROSS
13000     goto CALK_EXIT
13100 WATER: ! r: calculate water
13200     if x(12)>0 then let usage_water=x(12) ! water usage entered
13300     if x(9)><0 then let w(1)=x(9) : goto WATER_END ! water charge entered
13400     if b(1)><0 then let w(1)=b(1) : goto WATER_END ! standard water charge
13500     if d(13)<1 then let d(13)=1
13600     read #ratemst,using FORM_RATEMSTR, key="WA"&lpad$(str$(a(1)),2): mc1,mu1,mat rt nokey WATER_END
13700     let mu1=mu1*max(1,d(13))
13800     let mc1=mc1*max(1,d(13))
13900     let w(1)=mc1
14000     if usage_water>mu1 then let w(1)=w(1)+(usage_water-mu1)*rt(1,3)
14100 WATER_END: let g(1)=round(w(1),2)
14200     gosub WATER_USED
14300     return  ! /r
15000 SEWER: ! r: calculate sewer
15020 ! pr x$ : if trim$(x$)='100106.00' then pause
15040     if x(5)><0 then let w(2)=x(5) : goto SEWER_COMPLETED
15060     if b(2)><0 then let w(2)=b(2) : goto SEWER_COMPLETED
15080     read #ratemst,using FORM_RATEMSTR, key="SW"&lpad$(str$(a(2)),2): mc1,mu1,mat rt nokey SEWER_COMPLETED
15100     let mu1=mu1*max(1,d(13))
15120     let mc1=mc1*max(1,d(13))
15140 ! r: old chatom sewer calculation
15160 !   if usage_water=>mu1 then
15180 !     let w(2)=mc1+(usage_water-mu1)*rt(a(2),3)
15200 !   else 
15220 !     let w(2)=mc1
15240 !   end if
15260 ! /r
15280 ! r: new chatom sewer calculation - more standard... i hope
15300     let usage_sewer=usage_water
15310     let w(2)=mc1*max(1,extra(14)) ! units per meter - sewer (default to one)
15320     if usage_sewer<=mu1 then goto L3300 else let mu2=mu1
15340     for j=1 to 10
15360       if rt(j,1)>usage_sewer then goto L3300
15380       if usage_sewer<rt(j,2) then let w1=usage_sewer-mu2 else let w1=rt(j,2)-mu2
15400       let w(2)=w(2)+round(w1*rt(j,3),2)
15420       if rt(j,2)>usage_sewer then goto L3300
15440       let mu2=rt(j,2)
15460     next j
15480 L3300: ! 
15500 ! /r
15520 SEWER_COMPLETED: ! 
15540     let w(2)=round(w(2),2)
15560     let g(2)=w(2)
15580     return  ! /r
16000 ELECTRIC: ! r: NO ELECTRIC
16100     gosub ELECTRIC_USED
16200     return  ! /r
16400 GAS: ! r: NO GAS
16500     gosub GAS_USED
16600     return  ! /r
16700 SANITATION: ! r: Calculate sanitation
16800     if x(6)><0 then let w(5)=x(6) : goto SANITATION_END
16900     if b(5)><0 then let w(5)=b(5) : goto SANITATION_END
17000     read #ratemst,using FORM_RATEMSTR, key="SN"&lpad$(str$(a(5)),2): mc1,mu1,mat rt nokey SANITATION_END
17100     let w(5)=rt(a(5),3)
17200 SANITATION_END: let g(5)=w(5)
17300     return  ! /r
17400 FIRE: ! r: CALCULATE FIRE PROTECTION
17500     if x(7)><0 then let g(6)=x(7) : goto FIRE_END
17600     if b(6)><0 then let g(6)=b(6) : goto FIRE_END
17700 FIRE_END: return  ! /r
17800 MERCHANDISE: ! r:
17900     let g(7)=0
18000     return  ! /r
18100 OTHER_CHG: ! r: CALCULATE OTHER CHARGES
18200     if x(8)><0 then let g(8)=x(8) : goto OTHER_END
18300     if b(7)><0 then let g(8)=b(7) : goto OTHER_END
18400 OTHER_END: return  ! /r
18500 STAX: ! r: CALCULATE SALES TAX
18600     if a(6)=9 then goto STAX_END
18700     read #ratemst,using FORM_RATEMSTR, key="TX"&lpad$(str$(a(6)),2): mc1,mu1,mat rt nokey STAX_END
18800     let g(9)=round(g(1)*rt(a(6),3),2)
18900 STAX_END: return  ! /r
19000 PENALTY: ! r:
19002     if a(7)=9 then goto PENALTY_END ! CALCULATE PENALTY
19100     let a(7)=1 ! defalt to code 1
19200     read #ratemst,using FORM_RATEMSTR, key="PN"&lpad$(str$(a(7)),2): mc1,mu1,mat rt nokey PENALTY_END
19300     let g(10)=mc1 ! standard penalty
19400 PENALTY_END: return  ! /r
19500 NET_GROSS: ! r: calculate net & gross bill
19600     for j=1 to 10
19700       if uprc$(penalty$(j))><"Y" then let g(11)=g(11)+g(j) ! don't add penalties into net
19800       let g(12)=g(12)+g(j)
19900     next j
20000     return  ! /r
20100 WATER_USED: ! r: updates water readings & usages
20200     if d1><f then let d(2)=d(1)
20300     let d(1)=x(1)
20400     let w8=d(3)
20500     let d(3)=usage_water
20600     let d(4)=d(4)+d(3)
20700     if d1=f then let d(4)=d(4)-w8
20800     return  ! /r
20900 ELECTRIC_USED: ! r: updates electric readings & usages
21000     if d1><f then let d(6)=d(5)
21100     let d(5)=x(3)
21200     let w8=d(7)
21300     let d(7)=x2
21400     let d(8)=d(8)+d(7)
21500     if d1=f then let d(8)=d(8)-w8
21600     return  ! /r
21700 GAS_USED: ! r: updates gas readings & usages
21800     if d1><f then let d(10)=d(9)
21900     let d(9)=x(2)
22000     let w8=d(11)
22100     let d(11)=x3
22200     let d(12)=d(12)+d(11)
22300     if d1=f then let d(12)=d(12)-w8
22400     return  ! /r
22500 CALK_EXIT: ! 
22600   fnend 
