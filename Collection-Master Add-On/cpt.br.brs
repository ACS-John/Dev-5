fn_setup(table$)
on error goto Ertn
! pr 'table$=';table$
fntop(program$)
if ~exists('[Q]\Data\cpt.dat') then let fn_InitialializeCpt
fnHamsterFio(table$)
XIT: !
fnxit
def fn_setup(&table$)
	if ~setup then
		setup=1
		library 'S:\Core\Library': fnTop
		library 'S:\Core\Library': fnXit
		library 'S:\Core\Library': fnHamsterFio
		library 'S:\Core\Library': fnOpenFile
		library 'S:\Core\Library': fnCloseFile
		library 'S:\Core\Library': fnKeyExists
		library 'S:\Core\Library': fnCopy
		dim cpt$(0)*1024
		dim cptN(0)
		table$='CM CPT'
	end if
fnend
def fn_InitialializeCpt
	 hTable=fn_open(table$,mat cpt$,mat cptN,mat form$)
	! r: initial list of CPT codes
	fn_addCpt(hTable,'76942',"ULTRASONIC GUIDANCE FOR NEEDLE PLACEMENT (EG, BIOPSY, ASPIRATION, INJECTION, LOC")
	fn_addCpt(hTable,'78452',"MULTIPLE STUDIES, AT REST &/OR STRESS &/OR REDISTRIBUTION &/OR REST REINJECTION")
	fn_addCpt(hTable,'92920',"Balloon dilation of narrowed or blocked major coronary artery or branch (accessed through the skin)")
	fn_addCpt(hTable,'92921',"PERCUTANEOUS TRANSLUMINAL CORONARY ANGIOPLASTY; EACH ADDITIONAL BRANCH OF A MAJ")
	fn_addCpt(hTable,'92928',"PRECUTANEOUS TRANSCATHETER PLACEMETN OF INTRACORONARY STENT, W\ CORONARY ANGIO")
	fn_addCpt(hTable,'92933',"Removal of plaque and insertion of stent in major coronary artery or branch, accessed through the skin")
	fn_addCpt(hTable,'92943',"Insertion of stent, removal of plaque and/or balloon dilation of coronary vessel, accessed through the skin")
	fn_addCpt(hTable,'92973',"Removal of blood clot in heart artery, accessed through the skin")
	fn_addCpt(hTable,'92978',"INTRAVASCULAR ULTRASOUND (CORONARY VESSEL OR GRAFT) DURING DIAGNOSTIC EVALUATION")
	fn_addCpt(hTable,'93010',"ELECTROCARDIOGRAM, ROUTINE ECG WITH AT LEAST 12 LEADS; INTERPRETATION AND REPORT")
	fn_addCpt(hTable,'93016',"CARDIOVASCULAR STRESS TEST USING MAXIMAL OR SUBMAXIMAL TREADMILL OR BICYCLE EXERCISE, CONTINUOUS ELECTROCARDIOGRAPHIC MONITORING, AND/OR PHARMACOLOGICAL STRESS; PHYSICIAN SUPERVISION ONLY, WITHOUT INTERPRETATION AND REPORT")
	fn_addCpt(hTable,'93018',"CARDIOVASCULAR STRESS TEST USING MAXIMAL OR SUBMAXIMAL TREADMILL OR BICYCLE EXER")
	fn_addCpt(hTable,'93280',"Evaluation, testing, and programming adjustment of permanent dual lead pacemaker system with physician analysis, review, and report")
	fn_addCpt(hTable,'93283',"Evaluation, testing and programming adjustment of defibrillator with analysis, review and report")
	fn_addCpt(hTable,'93306',"ECHOCARDIOGRAPHY, TRANSTHORACIC, REAL-TIME W/ IMAGE DOC")
	fn_addCpt(hTable,'93451',"Insertion of catheter for diagnostic evaluation of right heart structures")
	fn_addCpt(hTable,'93454',"Catheter placement in coronary artery(s) for coronary angiography, including intraprocedural injection(s) for coronary angiography, imaging supervision and interpretation;")
	fn_addCpt(hTable,'93455',"Catheter placement in coronary artery(s) for coronary angiography, including intraprocedural injection(s) for coronary angiography, imaging supervision and interpretation; with catheter placement(s) in bypass graft(s) (internal mammary, free arterial, venous grafts) including intraprocedural injection(s) for bypass graft angiography")
	fn_addCpt(hTable,'93458',"Catheter placement in coronary artery(s) for coronary angiography, including intraprocedural injection(s) for coronary angiography, imaging supervision and interpretation; with left heart catheterization including intraprocedural injection(s) for left ventriculography, when performed")
	fn_addCpt(hTable,'93459',"Catheter placement in coronary artery(s) for coronary angiography, including intraprocedural injection(s) for coronary angiography, imaging supervision and interpretation; with left heart catheterization including intraprocedural injection(s) for left ventriculography, when performed, catheter placement(s) in bypass graft(s) (internal mammary, free arterial, venous grafts) with bypass graft angiography")
	fn_addCpt(hTable,'93460',"Catheter placement in coronary artery(s) for coronary angiography, including intraprocedural injection(s) for coronary angiography, imaging supervision and interpretation; with right and left heart catheterization including intraprocedural injection(s) for left ventriculography, when performed")
	fn_addCpt(hTable,'93461',"Catheter placement in coronary artery(s) for coronary angiography, including intraprocedural injection(s) for coronary angiography, imaging supervision and interpretation; with right and left heart catheterization including intraprocedural injection(s) for left ventriculography, when performed, catheter placement(s) in bypass graft(s) (internal mammary, free arterial, venous grafts) with bypass graft angiography")
	fn_addCpt(hTable,'93503',"Insertion and placement of flow directed catheter (eg, Swan-Ganz) for monitoring purposes")
	fn_addCpt(hTable,'93567',"Injection procedure during cardiac catheterization including imaging supervision, interpretation, and report; for supravalvular aortography (List separately in addition to code for primary procedure)")
	fn_addCpt(hTable,'93571',"Intravascular Doppler velocity and/or pressure derived coronary flow reserve measurement (coronary vessel or graft) during coronary angiography including pharmacologically induced stress; initial vessel (List separately in addition to code for primary procedure)")
	fn_addCpt(hTable,'93572',"Intravascular Doppler velocity and/or pressure derived coronary flow reserve measurement (coronary vessel or graft) during coronary angiography including pharmacologically induced stress; each additional vessel (List separately in addition to code for primary procedure)")
	fn_addCpt(hTable,'96374',"Therapeutic, prophylactic, or diagnostic injection (specify substance or drug); intravenous push, single or initial substance/drug")
	fn_addCpt(hTable,'99205',"Office or other outpatient visit for the evaluation and management of a new patient, which requires these 3 key components: A comprehensive history; A comprehensive examination; Medical decision making of high complexity. Counseling and/or coordination of care with other physicians, other qualified health care professionals, or agencies are provided consistent with the nature of the problem(s) and the patient's and/or family's needs. Usually, the presenting problem(s) are of moderate to high severity. Typically, 60 minutes are spent face-to-face with the patient and/or family.")
	fn_addCpt(hTable,'99220',"Initial observation care, per day, for the evaluation and management of a patient, which requires these 3 key components: A comprehensive history; A comprehensive examination; and Medical decision making of high complexity. Counseling and/or coordination of care with other physicians, other qualified health care professionals, or agencies are provided consistent with the nature of the problem(s) and the patient's and/or family's needs. Usually, the problem(s) requiring admission to outpatient hospital ""observation status"" are of high severity. Typically, 70 minutes are spent at the bedside and on the patient's hospital floor or unit.")
	fn_addCpt(hTable,'99221',"Initial hospital care, per day, for the evaluation and management of a patient,Typically, 30 minutes are spent at the bedside and on the patient's hospital floor or unit. ")
	fn_addCpt(hTable,'99222',"Initial hospital care, per day, for the evaluation and management of a patient, Typically, 50 minutes are spent at the bedside and on the patient's hospital floor or unit.")
	fn_addCpt(hTable,'99223',"Initial hospital care, per day, for the evaluation and management of a patient, Typically, 70 minutes are spent at the bedside and on the patient's hospital floor or unit.")
	fn_addCpt(hTable,'99225',"Subsequent observation care, per day, for the evaluation and management of a patient, ypically, 25 minutes are spent at the bedside and on the patient's hospital floor or unit.")
	fn_addCpt(hTable,'99226',"Subsequent observation care, per day, for the evaluation and management of a patient, typically, 35 minutes are spent at the bedside and on the patient's hospital floor or unit.")
	fn_addCpt(hTable,'99231',"Subsequent hospital care, per day, for the evaluation and management of a patient, Typically, 15 minutes are spent at the bedside and on the patient's hospital floor or unit.")
	fn_addCpt(hTable,'99232',"Subsequent hospital care, per day, for the evaluation and management of a patient, Typically, 25 minutes are spent at the bedside and on the patient's hospital floor or unit.")
	fn_addCpt(hTable,'99233',"Subsequent hospital care, per day, for the evaluation and management of a patient, Typically, 35 minutes are spent at the bedside and on the patient's hospital floor or unit.")
	fn_addCpt(hTable,'99236',"Observation or inpatient hospital care, for the evaluation and management of a patient including admission and discharge on the same date, ")
	fn_addCpt(hTable,'99238',"Hospital discharge day management; 30 minutes or less")
	fn_addCpt(hTable,'99239',"Hospital discharge day management; more than 30 minute")
	fn_addCpt(hTable,'99254',"Inpatient consultation for a new or established patient,")
	fn_addCpt(hTable,'99255',"Inpatient consultation for a new or established patient,")
	fn_addCpt(hTable,'99284',"Emergency department visit for the evaluation and management of a patient,")
	fn_addCpt(hTable,'99285',"Emergency department visit for the evaluation and management of a patient,")
	fn_addCpt(hTable,'99291',"Critical care, evaluation and management of the critically ill or critically injured patient; first 30-74 minutes")
	fn_addCpt(hTable,'99292',"Critical care, evaluation and management of the critically ill or critically injured patient; each additional 30 minutes (List separately in addition to code for primary service)")
	fn_addCpt(hTable,'99406',"Smoking and tobacco use cessation counseling visit; intermediate, greater than 3 minutes up to 10 minutes")
	fn_addCpt(hTable,'99407',"Smoking and tobacco use cessation counseling visit; intensive, greater than 10 minutes")
	fn_addCpt(hTable,'G0181',"")
	! /r
	fnCloseFile(hTable,table$)
	fn_InitialializeCpt=hLocation
fnend
def fn_addCpt(hCpt,code$*5,desc$*1024) ! requires local form$ and cpt_ enumerations as well as setup for mat cpt$ amd mat cptN
	mat cpt$=('')
	mat cptN=(0)
	cpt$(cpt_code)=code$
	cpt$(cpt_desc)=desc$
	read #hCpt,using form$(hCpt),key=code$,release: mat cpt$,mat cptN nokey AddCptAdd
	pr 'attempted to add code '&code$&', but it already existed. - skipped instead';bell
	goto AddCptXit
	AddCptAdd: !
	write #hCpt,using form$(hCpt): mat cpt$,mat cptN
	goto AddCptXit
	AddCptXit: !
fnend



def library fnCptCode$*800(code$*5)
	if ~setup then let fn_setup(table$)
	if ~setup_fnCptCode then
		setup_fnCptCode=1
		hFnCptCode=fn_open(table$,mat cpt$,mat cptN,mat form$)
	end if
	cpt$(cpt_code)=code$
	mat cpt$=('')
	mat cptN=(0)
	read #hFnCptCode,using form$(hFnCptCode),key=rpad$(code$,kln(hFnCptCode)): mat cpt$,mat cptN nokey ignore
	fnCptCode$=cpt$(cpt_desc)
fnend

include: fn_open
include: ertn