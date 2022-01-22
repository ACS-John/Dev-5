! Replace S:\acsGL\Conversion\FinStmt_v0_to_v1
! converts fin stmts files (all of them)  from recl=79 to recl=83 and version 1
def library fnfinstmt_v0_to_v1
	autoLibrary
	on error goto Ertn

	fnStatus('Converting Financial Statement.')
	dim fil$(6),idx$(6)
	fil$(1)="acglfnSB" : idx$(1)="agfsidx4" ! Balance Sheet
	fil$(2)="acglfnSI" : idx$(2)="agfsidx3" ! Income Statement
	fil$(3)="acglfnSF" : idx$(3)="agfsidx5" ! Funt Statement / Cash Flow
	fil$(4)="acglfnSC" : idx$(4)="agfsidx1" ! Secondary Balance Sheet
	fil$(5)="acglfnSJ" : idx$(5)="agfsidx2" ! Secondary Income Statement
	fil$(6)="acglfnSG" : idx$(6)="agfsidx6" ! Secondary Fund / Cash Flow

	for j=1 to 6
	fnCopy('[Q]\GLmstr\'&fil$(j)&'.h[cno]','[Q]\GLmstr\"&fil$(j)&".h[cno]', 83)
	fnRemoveDeletedRecords('[Q]\GLmstr\'&fil$(j)&'.h[cno]')
		fnIndex('[Q]\GLmstr\'&fil$(j)&'.h[cno]','[Q]\GLmstr\'&idx$(j)&'.h[cno]','1 5')

		if j=2 or j=5 then 
			open #hgl=fnH: "Name=[Q]\GLmstr\"&fil$(j)&".h[cno],KFName=[Q]\GLmstr\"&idx$(j)&".h[cno]",i,outIn,k 
			version(1,1)
			delete_count=read_count=0
			end1=st1=st2=rno=rnp=0
			PHASE1: ! gosub FIND1
			st1=rno : st2=99999 : rnp=0
			READ_1: ! 
			read #hgl,using 'form pos 1,G 5,pos 75,N 1': rno,ic eof PHASE1_EOF,conv PHASE1_READ_CONV
			read_count+=1
			pr 'read_1 did'; ! pause
			if rno=0 then delete #hgl: : goto READ_1
			if ic=0 then pr ' ic=0, skipped' : goto READ_1
			if ic=1 then pr ' rnp=rno' : rnp=rno
			if ic=2 then pr ' st2=rno' : st2=rno : goto PHASE2
			pr ' ic=';ic;', skipped'
			goto READ_1

			PHASE1_READ_CONV: ! 
			!     pr 'PHASE1_READ_CONV,read_count=';read_count : pause
			read #hgl,using 'form pos 1,C 5,pos 75,N 1': rno$ eof PHASE1_EOF
			delete #hgl: 
			delete_count+=1
			goto READ_1

			PHASE1_EOF: ! 
			!   pr 'PHASE1_EOF,read_count=';read_count : pause
			end1=1
			PHASE2: ! 
			pr 'restoring to ';st1
			!   pr 'PHASE2,read_count=';read_count : pause
			restore #hgl,key>=lpad$(str$(st1),5): nokey PHASE2_EOF
			do 
				read #hgl,using 'form pos 1,G 5,pos 75,N 1': rno,ic eof PHASE2_EOF
				pr 'do read did';
				!     if end1=1 then pr 'end1=1' : pause ! goto PHASE2_EOF
				if rno<st2 then pr 'going back to L390' : goto L390
				if end1=1 then goto PHASE2_EOF
				rnp=0
				goto PHASE1
				L390: !
				rewrite #hgl,using 'form pos 79,N 5': rnp
			!     pr 'rewrite did' : pause
			loop 
			PHASE2_EOF: ! 
			close #hgl: 
			hgl=0
		end if  ! j=2 or j=5
		NEXT_J: ! 
	next j
goto Xit

Xit: fnend 
include: ertn
