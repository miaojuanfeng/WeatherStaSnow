!program Main_Day
!    use JL_Mod_StaSnow_Common
!    use JL_Mod_StaSnow_Day
!    implicit none
!    character*(FileNameMaxLen):: HisFileName
!    integer:: StaID,Month,Day
!    integer:: StartYear,EndYear
!    
!    StaID=50353
!    StartYear=1951
!    EndYear=2010
!    Month=3
!    Day=10
!    
!    call DayDepthSort(StaID,Month,Day,StartYear,EndYear)
!!    call CreateDayDepthSortFileName(HisFileName,StaID,Month,Day)
!!    write(*,*)HisFileName
!end program Main_Day