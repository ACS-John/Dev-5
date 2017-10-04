 ! function\exportlistviewcsv.brs
 ! Created on 07/03/2014
 !
 ! fnExportListviewCSV - This function exports a Listview in its current
 !  form to a CSV file, using a new function in FileIO.
 !
 ! You must specify the listviews Control Name in UserData
 !
 !
 def fnPrintListview(;___,ListviewSubscript)
    let ListviewSubscript=fnFindSubscript(mat Subscripts$,"ctl_",UserData$(ControlIndex))
    if ListviewSubscript then
       fnExportListviewCSV(Window,fnListSpec$(ControlSpec$(ListviewSubscript)),1)
    else
       let msgbox("Couldn't find the target listview. Please ensure a valid listview is selected for the pr button, or contact the programmer who worked on your system to fix this error.")
    end if
 fnend
