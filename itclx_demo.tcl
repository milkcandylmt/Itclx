# 总结① 继承的使用，应当在上层去自动处理子类的实现，最多在基类中设置一个属性表明子类
#     ② 继承体系中，应当主要看基类，子类只要需要时才看
#     ③ 委托、依赖关系中，底层中应该不存在对上层的调用
#     ④ 不得已时，不要将属性作为public级别公开
package require itclx


proc CLASS-Movie {} {}
class Price {
  protected variable _priceCode 0
  
  public method getCharge     ;# 抽象函数，在子类中实现
  public method getFrequentRenterPoints
}
proc Price-::getFrequentRenterPoints {daysRented} {
  return 1
}
classEnd Price


proc CLASS-ChildrensPrice {} {}
class ChildrensPrice {
  inherit Price
  
  public method getCharge
  constructor {priceCode} {set _priceCode $priceCode}
}
proc ChildrensPrice-::getCharge {daysRented} {
  set result 1.5
  if {$daysRented>3} {  set result [expr {$result+($daysRented-3)*1.5}]  }
  return $result
}
classEnd ChildrensPrice


proc CLASS-NewReleasePrice {} {}
class NewReleasePrice {
  inherit Price
  
  public method getCharge
  public method getFrequentRenterPoints
  constructor {priceCode} {set _priceCode $priceCode}
}
proc NewReleasePrice-::getCharge {daysRented} {
  return [expr $daysRented*3]
}
proc NewReleasePrice-::getFrequentRenterPoints {daysRented} {
  if {$daysRented>1} {return 2} else {return 1}
}
classEnd NewReleasePrice


proc CLASS-RegularPrice {} {}
class RegularPrice {
  inherit Price
  
  public method getCharge
  constructor {priceCode} {set _priceCode $priceCode}
}
proc RegularPrice-::getCharge {daysRented} {
  set result 2
  if {$daysRented>2} {  set result [expr {$result+($daysRented-2)*1.5}]  }
  return $result
}
classEnd RegularPrice


proc CLASS-Movie {} {}
class Movie {
  public common CHILDRENS 2
  public common REGULAR 0
  public common NEW_RELEASE 1
  private variable _title ""
  private variable _price ""
  
  private method _setPriceCode
  public method getCharge
  public method getFrequentRenterPoints
  
  constructor {title priceCode} {
    set _title $title
    _setPriceCode $priceCode
  }
}
proc Movie-::_setPriceCode {priceCode} {
  #puts "priceCode=$priceCode， {$REGULAR $CHILDRENS $NEW_RELEASE}"
  switch $priceCode \
    $REGULAR     {set _price [RegularPrice ::#auto $REGULAR]}         \
    $CHILDRENS   {set _price [ChildrensPrice ::#auto $CHILDRENS]}     \
    $NEW_RELEASE {set _price [NewReleasePrice ::#auto $NEW_RELEASE]}  \
    default     {
      error "错误的priceCode={$priceCode}，须属于{$REGULAR $CHILDRENS $NEW_RELEASE}"
    }
}
proc Movie-::getCharge {daysRented} {
  return [$_price getCharge $daysRented]
}
proc Movie-::getFrequentRenterPoints {daysRented}  {
  return [$_price getFrequentRenterPoints $daysRented]
}
classEnd Movie


proc CLASS-Rental {} {}
class Rental {
  private variable _movie       ""       ;# 原例中，定义为Movie类类型的实例变量
  private variable _daysRented  ""
  
  public method getCharge
  public method getFrequentRenterPoints
  
  constructor {movie daysRented} {
    set _movie $movie             ;# 实例变量操作，得到新的实例名
    set _daysRented $daysRented
  }
}
proc Rental-::getCharge {} {
  return [$_movie getCharge $_daysRented]  ;#【【】】
}
proc Rental-::getFrequentRenterPoints {} {
  return [$_movie getFrequentRenterPoints $_daysRented]  ;#【【】】
}
classEnd Rental


proc CLASS-User {} {}
class User {
  private variable _name    ""
  private variable _rentals ""
  
  public method AddRental
  public method getStatement
  private method getTotalCharge
  private method getTotalFrequentRenterPoints 
  
  constructor {name} {set _name $name}
}
proc User-::AddRental {rental} {
  lappend _rentals $rental
}
proc User-::getStatement {} {
    # 原例中，Enumeration是枚举类，elements()是取数组中元素
  set rentals $_rentals  
  set result "Rental Record for $_name\n"
  foreach rental $rentals {
    # show figures for this rental
    append result "\t[oo $rental._movie._title]\t[$rental getCharge]\n" ;# 【】
  }
  ;
  # add footer lines
  append result "Amount owed is [getTotalCharge]\n"
  append result "You earned [getTotalFrequentRenterPoints] frequent renter points"
  return $result
}
;
proc User-::getTotalCharge {} {
  set result 0
  foreach rental $_rentals {  set result [expr $result+[$rental getCharge]]  }
  return $result
}
;
proc User-::getTotalFrequentRenterPoints {} {
  set result 0
  foreach rental $_rentals {  set result [expr $result+[$rental getFrequentRenterPoints]]  }
  return $result
}
;
classEnd User


proc 1_Main {} {}
    # 定义已有影片数据 {title priceCode},  
    # priceCode的0/1/2分别代表普通片/新片/儿童片
    # 1->(days*3)   0->(days>2?2+(days-2)*1.5:2)  2->(days>3?1.5+(1.5-3)*1.5:1.5)
  set movie0 [New Movie "故事会" 0]
  set movie01 [New Movie "故事会1" 0]
  set movie1 [New Movie "新西游" 1]
  set movie11 [New Movie "新三国" 1]
  set movie2 [New Movie "童话集" 2]
  set movie21 [New Movie "童话集1" 2]
  # Movie movie_3 "C语言"  3  ;# 应报错，不存在的价格代码
  
    # 新增用户1，他订阅影片0,1,2
  set user1 [New User name1]      ;# 新增一个用户
  set daysRentedDc "$movie0 1 $movie01 3 $movie1 2 $movie11 2 $movie2 3 $movie21 6"
  foreach {i_movie i_daysRented} $daysRentedDc {
      ;# set i_movie $movie0; set i_daysRented 1
    set i_rental [New Rental $i_movie $i_daysRented] 
    $user1 AddRental $i_rental
  }
  ;
  $user1 getStatement



