subroutine ba (dbh, expf, ht, species, dbhmin, dbhmax, htmin, htmax, &
ntree, ba_)
use constants
implicit none

!###############################################################################
!This subroutine calculates a basal area per acre given input vectors containing
!diameter at breast height and expansion factor values. This attribute can be 
!calculated for user defined size ranges and for select species.
!###############################################################################

!Arguments
integer, intent(in) :: ntree, species(ntree)
real(real64), intent(in) :: dbh(ntree), expf(ntree), ht(ntree)
real(real64), intent(in) :: dbhmin, dbhmax, htmin, htmax
real(real64), intent(out) :: ba_
real(real64) :: dbh_, expf_, ht_
integer :: i, species_

!intialize ba_ to 0
ba_ = 0.0_real64

!Do the basal area calculation
do i = 1, ntree, 1

    dbh_ = dbh(i)
    expf_ = expf(i)
    ht_ = ht(i)
    species_ = species(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
    if(species_ < 1) cycle

    ba_ = ba_ + (dbh_*dbh_ * expf_ * f_con)

end do
end subroutine ba

!###############################################################################
!This subroutine calculates trees per acre given input vectors containing 
!diameter at breast height and expansion factor values. This attribute can be 
!calculated for user defined size ranges and for select species.
!###############################################################################

subroutine tpa (dbh, expf, ht, species, dbhmin, dbhmax, htmin, htmax, & 
ntree, tpa_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, species(ntree)
real(real64), intent(in) :: dbh(ntree), expf(ntree), ht(ntree)
real(real64), intent(in) :: dbhmin, dbhmax, htmin, htmax
real(real64), intent(out) :: tpa_
real(real64) :: dbh_, expf_, ht_
integer :: i, species_

!intialize tpa_ to 0
tpa_ = 0.0_real64

!Do the basal area calculation
do i = 1, ntree, 1

    dbh_ = dbh(i)
    expf_ = expf(i)
    ht_ = ht(i)
    species_ = species(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
    if(species_ < 1) cycle

    tpa_ = tpa_ + expf_

end do
end subroutine tpa

!###############################################################################
!This subroutine calculates quadratic mean diameter given input vectors 
!containing diameter at breast height and expansion factor values. This 
!attribute can be calculated for user defined size ranges and for select 
!species.
!###############################################################################

subroutine qmd (dbh, expf, ht, species, dbhmin, dbhmax, htmin, htmax, & 
ntree, qmd_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, species(ntree)
real(real64), intent(in) :: dbh(ntree), expf(ntree), ht(ntree)
real(real64), intent(in) :: dbhmin, dbhmax, htmin, htmax
real(real64), intent(out) :: qmd_
real(real64) :: dbh_, expf_, ht_,  tpa_, dbhsq
integer :: i, species_

!initialize variables
tpa_ = 0.0_real64
dbhsq = 0.0_real64
qmd_ = 0.0_real64

!Determine trees to include in QMD calculation
do i = 1, ntree, 1

    dbh_ = dbh(i)
    expf_ = expf(i)
    ht_ = ht(i)
    species_ = species(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
    if(species_ < 1) cycle

    dbhsq = dbhsq + dbh_*dbh_ * expf_
    tpa_ = tpa_ + expf_

end do

! Calculate QMD if TPA is greater than 0
if (tpa_ > 0.0_real64) qmd_ = sqrt(dbhsq / tpa_)

end subroutine qmd

!###############################################################################
!This subroutine calculates generalized mean diameter (Reineke diameter) given 
!input vectors containing diameter at breast height and expansion factor values.
!This attribute can be calculated for user defined size ranges and for select 
!species.
!###############################################################################

subroutine gmd (dbh, expf, ht, species, dbhmin, dbhmax, htmin, htmax, & 
ntree, gmd_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, species(ntree)
real(real64), intent(in) :: dbh(ntree), expf(ntree), ht(ntree)
real(real64), intent(in) :: dbhmin, dbhmax, htmin, htmax
real(real64), intent(out) :: gmd_
real(real64) :: dbh_, expf_, ht_, tpa_, gmd_sum
integer :: i, species_

!initialize variables
tpa_ = 0.0_real64
gmd_sum = 0.0_real64
gmd_ = 0.0_real64

!Determine trees to include in GMD calculation
do i = 1, ntree, 1

    dbh_ = dbh(i)
    expf_ = expf(i)
    ht_ = ht(i)
    species_ = species(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
    if(species_ < 1) cycle

    gmd_sum = gmd_sum + dbh_**r_slope * expf_
    tpa_ = tpa_ + expf_

end do

! Calculate GMD if TPA is greater than 0
if (tpa_ > 0.0_real64) gmd_ = (gmd_sum / tpa_)**(1.0_real64 / r_slope)

end subroutine gmd

!###############################################################################
!This subroutine calculates Lorey diameter (basal area weighted diameter) given
!input vectors containing diameter at breast height and expansion factor values.
!This attribute can be calculated for user defined size ranges and for select 
!species.
!###############################################################################

subroutine lorey_dia (dbh, expf, ht, species, dbhmin, dbhmax, htmin, &
htmax, ntree, lorey_dia_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, species(ntree)
real(real64), intent(in) :: dbh(ntree), expf(ntree), ht(ntree)
real(real64), intent(in) :: dbhmin, dbhmax, htmin, htmax
real(real64), intent(out) :: lorey_dia_
real(real64) :: dbh_, expf_, ht_, tpa_, dbh_sum, ba_, ba_tree
integer :: i, species_

!initialize variables
tpa_ = 0.0_real64
dbh_sum = 0.0_real64
lorey_dia_ = 0.0_real64
ba_ = 0.0_real64

!Determine trees to include in Lorey dia calculation
do i = 1, ntree, 1

    dbh_ = dbh(i)
    expf_ = expf(i)
    ht_ = ht(i)
    species_ = species(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
    if(species_ < 1) cycle

    ba_tree = dbh_*dbh_ * expf_ * f_con
    dbh_sum = dbh_sum + dbh_* ba_tree
    ba_ = ba_ + ba_tree

end do

! Calculate Lorey dia if A is greater than 0
if (ba_ > 0.0_real64) lorey_dia_ = (dbh_sum / ba_)

end subroutine lorey_dia

!###############################################################################
!This function is used to calculate QMD, GMD (reinekes diameter) or average 
!diameter weighted by TPA for the largest trees by DBH within a specified 
!percentage of TPA or an explicit TPA value. This value is calculated from a set
!of input vectors containing DBH values and expansion factors.
!###############################################################################

subroutine top_dia(dbh, sorted_idx, expf, top_tpa, top_per, dia_type, ntree,  &
top_dia_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, sorted_idx(ntree), dia_type
real(real64), intent(in) :: dbh(ntree), expf(ntree)
real(real64), intent(inout) :: top_tpa, top_per
real(real64), intent(out) :: top_dia_
real(real64) :: dbh_, expf_, tpa_, top, tpa_sum, dbh_sum
real(real64) :: expf_temp
integer :: i, idx
logical :: top_exceed

!Initialize variables
top_dia_ = 0.0_real64
tpa_ = 0.0_real64
tpa_sum = 0.0_real64
dbh_sum = 0.0_real64
expf_temp = 0.0_real64
top_exceed = .false.

!Do checks on top_tpa and top_per
if(top_tpa < 0.0_real64) top_tpa = 40.0_real64
if (top_per < 0.0_real64 .or. top_per > 100.0_real64) top_per = 20.0_real64

!Calculate TPA for stand
do i = 1, ntree, 1
    tpa_ = tpa_ + expf(i)
end do

!Do top diameter calculation if tpa_ > 0
if(tpa_ > 0.0_real64) then

    ! Define target "top" TPA
    if (top_per > 0.0_real64) then
        top = tpa_ * (top_per / 100.0_real64)
    else
        top = top_tpa    
    end if

    ! Constrain target top TPA to not exceed total stand TPA
    if (top > tpa_) top = tpa_

    !Determine trees to included in top diameter
    do i = 1, ntree, 1

        idx = sorted_idx(i)
        dbh_ = dbh(idx)
        expf_ = expf(idx)

        !Calculate how much TPA to take from this tree
        if (tpa_sum + expf_ >= top) then
            expf_temp = top - tpa_sum
            tpa_sum = top
            top_exceed = .true. 
        else
            expf_temp = expf_
            tpa_sum = tpa_sum + expf_temp
        end if

        !Update dbh_sum based on dia_type
        select case (dia_type)
        case(1)
            dbh_sum = dbh_sum + dbh_*dbh_ * expf_temp
        case(2)
            dbh_sum = dbh_sum + dbh_ * expf_temp
        case default 
            dbh_sum = dbh_sum + dbh_**r_slope * expf_temp
        end select

        !Break if top value has been exceeded
        if(top_exceed) exit

    end do
end if

!Calculate top diameter if tpa_sum > 0
if(tpa_sum > 0.0_real64) then
    select case(dia_type)
    case(1)
        top_dia_ = sqrt(dbh_sum / tpa_sum)
    case(2)
        top_dia_ = dbh_sum / tpa_sum
    case default
        top_dia_ = (dbh_sum / tpa_sum)**(1.0_real64 / r_slope)
    end select
end if

end subroutine top_dia

!###############################################################################
!This subroutine calculates Reinekes stand density index using the methodology 
!proposed by Stage 1968 from Section 7.3.2.1 of EFVS using input vectors 
!containing DBH and expansion factors. This attribute can be calculated for user
!defined size ranges and for select species.
!###############################################################################

subroutine rsdi_stage (dbh, expf, ht, species, dbhmin, dbhmax, htmin, &
htmax, ntree, rsdi_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, species(ntree)
real(real64), intent(in) :: dbh(ntree), expf(ntree), ht(ntree)
real(real64), intent(in) :: dbhmin, dbhmax, htmin, htmax
real(real64), intent(out) :: rsdi_
real(real64) :: dbh_, expf_, ht_, tpa_, dbhsq, a, b
integer :: i, species_

!intialize variables
rsdi_ = 0.0_real64
tpa_ = 0.0_real64
dbhsq = 0.0_real64

!Calculate tpa, dbhsq, and qmd for all trees
do i = 1, ntree, 1
    tpa_ = tpa_ + expf(i)
    dbhsq = dbhsq + dbh(i)**2.0_real64 * expf(i)
end do

!Do RSDI calculation if tpa_ > 0
if(tpa_ > 0.0_real64) then

    !Initialize a and b parameters
    a = 10**(-r_slope) * (1 - (r_slope/2)) * (dbhsq/tpa_)**(r_slope/2)
    b = 10**(-r_slope) * (r_slope/2) * (dbhsq/tpa_)**(r_slope/2 - 1)

    !Determine trees to include in RSDI calculation
    do i = 1, ntree, 1

        dbh_ = dbh(i)
        expf_ = expf(i)
        ht_ = ht(i)
        species_ = species(i)

        !Determine if tree should be skipped in calculation
        if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
        if(ht_ < htmin .or. ht_ >= htmax) cycle
        if(species_ < 1) cycle

        rsdi_ = rsdi_ + (a * expf_ + b * dbh_*dbh_ * expf_)
    end do
end if

end subroutine rsdi_stage

!###############################################################################
!This subroutine calculates stand density index using Zeide's method given input
!vectors containing diameter at breast height and expansion factor values. This 
!attribute can be calculated for user defined size ranges and for select 
!species.
!###############################################################################

subroutine zsdi (dbh, expf, ht, species, dbhmin, dbhmax, htmin, &
htmax, ntree, zsdi_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, species(ntree)
real(real64), intent(in) :: dbh(ntree), expf(ntree), ht(ntree)
real(real64), intent(in) :: dbhmin, dbhmax, htmin, htmax
real(real64), intent(out) :: zsdi_
real(real64) :: dbh_, expf_, ht_
integer :: i, species_

!initialize variables
zsdi_ = 0.0_real64

!Determine trees to include in ZSDI calculation
do i = 1, ntree, 1

    dbh_ = dbh(i)
    expf_ = expf(i)
    ht_ = ht(i)
    species_ = species(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
    if(species_ < 1) cycle

    zsdi_ = zsdi_ + ((dbh_ / 10.0_real64)**r_slope * expf_)

end do

end subroutine zsdi

!###############################################################################
!This subroutine calculates percent canopy cover corrected for overlap given 
!input vectors containing crown width and expansion factor values. This 
!attribute can be calculated for user defined size ranges and for select species.
!###############################################################################

subroutine cc (crwidth, dbh, expf, ht, species, dbhmin, dbhmax, & 
htmin, htmax, ntree, cc_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, species(ntree)
real(real64), intent(in) :: crwidth(ntree), dbh(ntree), expf(ntree), ht(ntree)
real(real64), intent(in) :: dbhmin, dbhmax, htmin, htmax
real(real64), intent(out) :: cc_
real(real64) :: crwidth_, dbh_, expf_, ht_
integer :: i, species_

!initialize variables
cc_ = 0.0_real64

!Determine trees to include in CC calculation
do i = 1, ntree, 1

    crwidth_ = crwidth(i)
    dbh_ = dbh(i)
    expf_ = expf(i)
    ht_ = ht(i)
    species_ = species(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
    if(species_ < 1) cycle

    cc_ = cc_ + ((crwidth_/2.0_real64)**2.0_real64 * &
     (expf_/43560.0_real64) * pi * 100.0_real64)

end do

!Correct CC for overlap
cc_ = correct_cc(cc_)

contains

    !###########################################################################
    !This function takes in an uncorrected percent canopy cover value and returns a 
    !corrected value using the relationship described on page 2 of Crookston, 
    !Nicholas L.; Stage, Albert R. 1999. Percent canopy cover and stand structure 
    !statistics from the Forest Vegetation Simulator. Gen. Tech. Rep. RMRS-GTR-24.
    !Ogden, UT: U. S. Department of Agriculture, Forest Service, Rocky Mountain 
    !Research Station. 11 p.
    !###########################################################################
    real(real64) function correct_cc (cc)
    implicit none

    !Arguments
    real(real64), intent(in) :: cc

    !Correct CC (Crookston & Stage 1999 overlap model)
    correct_cc = 100.0_real64 * (1.0_real64 - exp(-0.01_real64 * cc))

    end function correct_cc

end subroutine cc

!###############################################################################
!This subroutine calculates Lorey height using input vectors containing DBH,
!total tree height and expansion factors. This attribute can be calculated for
!user defined size ranges and for select species.
!###############################################################################

subroutine lorey_ht(dbh, expf, ht, species, dbhmin, dbhmax, htmin, &
htmax, ntree, lorey_ht_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, species(ntree)
real(real64), intent(in) :: dbh(ntree), expf(ntree), ht(ntree)
real(real64), intent(in) :: dbhmin, dbhmax, htmin, htmax
real(real64), intent(out) :: lorey_ht_
real(real64) :: dbh_, expf_, ht_, ba_, ba_tree, ba_sum
integer :: i, species_

!Initialize variables
lorey_ht_ = 0.0_real64
ba_ = 0.0_real64
ba_sum = 0.0_real64

!Determine trees to include in Lorey Height calculation
do i = 1, ntree, 1

    dbh_ = dbh(i)
    expf_ = expf(i)
    ht_ = ht(i)
    species_ = species(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
    if(species_ < 1) cycle

    ba_tree = dbh_*dbh_ * expf_ * f_con
    ba_sum = ba_sum + ba_tree * ht_
    ba_ = ba_ + ba_tree

end do

!Calculate lorey height if ba_ > 0
if(ba_ > 0.0_real64) lorey_ht_ = ba_sum / ba_

end subroutine lorey_ht

!###############################################################################
!This subroutine calculates top height using input vectors containing DBH,
!total tree height and expansion factors. This attribute can be calculated for
!user defined size ranges and for select species.
!###############################################################################

subroutine top_ht(sorted_idx, expf, ht, top_tpa, top_per, ntree, top_ht_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, sorted_idx(ntree)
real(real64), intent(in) :: expf(ntree), ht(ntree)
real(real64), intent(inout) :: top_tpa, top_per
real(real64), intent(out) :: top_ht_
real(real64) :: expf_, ht_, tpa_, top, tpa_sum, ht_sum, expf_temp
integer :: i, idx
logical :: top_exceed

!Initialize variables
top_ht_ = 0.0_real64
tpa_ = 0.0_real64
tpa_sum = 0.0_real64
ht_sum = 0.0_real64
top_exceed = .false.

!Do checks on top_tpa and top_per
if(top_tpa < 0.0_real64) top_tpa = 40.0_real64
if (top_per < 0.0_real64 .or. top_per > 100.0_real64) top_per = 20.0_real64

!Calculate tpa of stand
do i = 1, ntree, 1
    tpa_ = tpa_ + expf(i)
end do

!Do top height calculation if tpa > 0
if(tpa_ > 0.0_real64) then

    ! Define target "top" TPA.
    if (top_per > 0.0_real64) then
        top = tpa_ * (top_per / 100.0_real64)
    else
        top = top_tpa    
    end if

    !Constrain target top TPA to not exceed total stand TPA
    if (top > tpa_) top = tpa_

    !Determine trees to include in Top Height calculation
    do i = 1, ntree, 1

        idx = sorted_idx(i)
        expf_ = expf(idx)
        ht_ = ht(idx)

        !Calculate how much TPA to take from this tree and update tpa_sum
        if (tpa_sum + expf_ >= top) then
            expf_temp = top - tpa_sum
            tpa_sum = top
            top_exceed = .true.
        else
            expf_temp = expf_
            tpa_sum = tpa_sum + expf_temp
        end if

        !Update ht_sum
        ht_sum = ht_sum + (ht_ * expf_temp)

        if (top_exceed) exit

    end do

    !Calculate top height if tpa_sum > 0
    if(tpa_sum > 0.0_real64) top_ht_ = ht_sum / tpa_sum
end if

end subroutine top_ht

!###############################################################################
!This subroutine calculates basal area in trees larger than subject tree using 
!input vectors containing DBH and expansion factors. 
!###############################################################################

subroutine bal(dbh, sorted_idx, expf, no_ties, ntree, bal_arr)
use constants
implicit none 

integer, intent(in) :: ntree, sorted_idx(ntree), no_ties
real(real64), intent(in) :: dbh(ntree), expf(ntree)
real(real64), intent(out) :: bal_arr(ntree)
real(real64) :: dbh_, expf_, temp_dbh, temp_bal, bal_sum, ba_tree
integer :: i, idx

!Initialize variables
bal_arr = 0.0_real64
bal_sum  = 0.0_real64
temp_bal = 0.0_real64
temp_dbh = 10000.0_real64
ba_tree = 0.0_real64

!Branch to determine calculation method
select case(no_ties)

! Handle ties in DBH values (Trees with same DBH get same BAL)
case(0) 
    do i = 1, ntree, 1
        idx = sorted_idx(i)
        dbh_ = dbh(idx)
        ba_tree = dbh_*dbh_ * expf(idx) * f_con

        if(dbh_ < temp_dbh) then
            bal_arr(idx) = bal_sum
            temp_dbh = dbh_
            temp_bal = bal_sum
            bal_sum = bal_sum + ba_tree
        else
            ! DBH values are exactly equal. Share the previous tree's BAL.
            bal_arr(idx) = temp_bal
            bal_sum = bal_sum + ba_tree     
        end if     
    end do    

! Ignore ties (all trees get different BAL)
case default 
    do i = 1, ntree, 1
        idx = sorted_idx(i)
        ba_tree = dbh(idx)*dbh(idx) * expf(idx) * f_con
        
        bal_arr(idx) = bal_sum
        bal_sum = bal_sum + ba_tree
    end do

end select

end subroutine bal

!###############################################################################
!This function sums and expands an input numeric attribute to a per unit area
!basis using numeric vectors containing diameter, attribute of interest, and 
!expansion factors. The numeric attribute could be a tree-level volume, 
!biomass, carbon, etc. This attribute can be calculated for user defined size 
!ranges and for select species.
!###############################################################################

subroutine expand_attr(attr, expf, dbh, ht, species, dbhmin, dbhmax, htmin, &
htmax, ntree, expand_attr_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, species(ntree)
real(real64), intent(in) :: dbh(ntree), ht(ntree), attr(ntree), expf(ntree)
real(real64), intent(in) :: dbhmin, dbhmax, htmin, htmax
real(real64), intent(out) :: expand_attr_
real(real64) :: dbh_, expf_, ht_, attr_
integer :: i, species_

!Initialize variables
expand_attr_ = 0.0_real64

!Determine trees to include in attribute sum
do i = 1, ntree, 1

    attr_ = attr(i)
    expf_ = expf(i)
    dbh_ = dbh(i)
    ht_ = ht(i)
    species_ = species(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
    if(species_ < 1) cycle

    expand_attr_ = expand_attr_ + (attr_ * expf_)

end do

end subroutine expand_attr

!###############################################################################
! This function is used to calculate the arithmetic or weighted mean (average) 
! of an attribute. The weighted mean will only be calculated if weights are 
! provided as an input argument. These mean values can be calculated within 
! custom size ranges and for select species.
!###############################################################################

subroutine mean_attr(attr, weight, dbh, ht, species, dbhmin, dbhmax, htmin, &
htmax, ntree, mean_attr_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, species(ntree)
real(real64), intent(in) :: dbh(ntree), ht(ntree), attr(ntree), weight(ntree)
real(real64), intent(in) :: dbhmin, dbhmax, htmin, htmax
real(real64), intent(out) :: mean_attr_
real(real64) :: dbh_, attr_, ht_, weight_, weight_sum, attr_sum
integer :: i, species_

!Initialize variables
mean_attr_ = 0.0_real64
weight_sum = 0.0_real64
attr_sum = 0.0_real64

!Determine trees to include in mean calculation
do i = 1, ntree, 1

    attr_ = attr(i)
    weight_ = weight(i)
    dbh_ = dbh(i)
    ht_ = ht(i)
    species_ = species(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
    if(species_ < 1) cycle

    attr_sum = attr_sum + (attr_ * weight_)
    weight_sum = weight_sum + weight_

end do

!Calculate average
if(weight_sum > 0.0_real64) mean_attr_ = attr_sum / weight_sum

end subroutine mean_attr

!###############################################################################
!This function counts the number of tree records between specified DBH and HT
!ranges and for select species.
!###############################################################################

subroutine count_rec(dbh, ht, species, dbhmin, dbhmax, htmin, &
htmax, ntree, count_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, species(ntree)
real(real64), intent(in) :: dbh(ntree), ht(ntree)
real(real64), intent(in) :: dbhmin, dbhmax, htmin, htmax
real(real64), intent(out) :: count_
real(real64) :: dbh_, ht_
integer :: i, species_

!Initialize variables
count_ = 0.0_real64

!Determine trees to include in count
do i = 1, ntree, 1

    dbh_ = dbh(i)
    ht_ = ht(i)
    species_ = species(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
    if(species_ < 1) cycle

    count_ = count_ + 1.0_real64

end do

end subroutine count_rec

!###############################################################################
!This function determines the minimum value for an input attribute. This can 
!be calculated for custom size ranges and for select species.
!###############################################################################

subroutine min_attr(attr, dbh, ht, species, dbhmin, dbhmax, htmin, &
htmax, ntree, min_attr_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, species(ntree)
real(real64), intent(in) :: dbh(ntree), ht(ntree), attr(ntree)
real(real64), intent(in) :: dbhmin, dbhmax, htmin, htmax
real(real64), intent(out) :: min_attr_
real(real64) :: dbh_, attr_, ht_
integer :: i, species_
logical :: first_match

!Initialize variables
min_attr_ = 0.0_real64
first_match = .true.

!Look for min
do i = 1, ntree, 1

    dbh_ = dbh(i)
    species_ = species(i)
    ht_ = ht(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
    if(species_ < 1) cycle

    attr_ = attr(i)

    ! Handle the first valid tree encountered vs subsequent comparisons
    if (first_match) then
        min_attr_ = attr_
        first_match = .false.
    else
        if (attr_ < min_attr_) min_attr_ = attr_
    end if

end do

end subroutine min_attr

!###############################################################################
!This function determines the maximum value for an input attribute. This can 
!be calculated for custom size ranges and for select species.
!###############################################################################

subroutine max_attr(attr, dbh, ht, species, dbhmin, dbhmax, htmin, &
htmax, ntree, max_attr_)
use constants
implicit none

!Arguments
integer, intent(in) :: ntree, species(ntree)
real(real64), intent(in) :: dbh(ntree), ht(ntree), attr(ntree)
real(real64), intent(in) :: dbhmin, dbhmax, htmin, htmax
real(real64), intent(out) :: max_attr_
real(real64) :: dbh_, attr_, ht_
integer :: i, species_
logical :: first_match

!Initialize variables
max_attr_ = 0.0_real64
first_match = .true.

!Look for max
do i = 1, ntree, 1

    dbh_ = dbh(i)
    species_ = species(i)
    ht_ = ht(i)

    !Determine if tree should be skipped in calculation
    if(dbh_ < dbhmin .or. dbh_ >= dbhmax) cycle
    if(ht_ < htmin .or. ht_ >= htmax) cycle
    if(species_ < 1) cycle

    attr_ = attr(i)

    ! Handle the first valid tree encountered vs subsequent comparisons
    if (first_match) then
        max_attr_ = attr_
        first_match = .false.
    else
        if (attr_ > max_attr_) max_attr_ = attr_
    end if

end do

end subroutine max_attr



