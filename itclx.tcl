# pkg_mkIndex -direct "lib/package/itclx" *.tcl
puts "itclx.tcl Initialed. when \[itclx::publicDebug on] , all oo operate will public" 

package require itcl


proc ---NMSP---itclx {} {}
namespace eval itclx {
  namespace export class classEnd oo New MsgTrust MsgSend Struct_Init Dim Lassign
  variable _PublicDebug 0   ;# 该值不为0时，所有通过oo的操作都为public权限
}
;

;# 设置_PublicDebug开关
proc itclx::publicDebug {{mode ""}} {
  variable _PublicDebug
  if {$mode==""} {return $_PublicDebug}
  if {[string tolower $mode] in {0 no off disable}} {
    set _PublicDebug 0
  } else {
    set _PublicDebug 1
  }
}
;

;# define,and create class. like itcl::class
proc itclx::_class {className clsBody} {
  set className [_nmspFull $className]
  set clsBody_ext [_class_extBody $className $clsBody]
  #puts "clsBody_ext={$clsBody_ext}"
  uplevel 1 [list namespace eval "$className-" {}]
  uplevel 1 [list itcl::class $className $clsBody_ext]
}
;

;# just define-class, havenot create it until use classEnd
proc itclx::class {className clsDefine} {
  set className [_nmspFull $className]
  if {[::itcl::find classes $className]!=""} {::itcl::delete class $className}
  set ::itclx::_ClassDefine($className) $clsDefine
  set ::itclx::_ClassBody($className) [_class_extBody $className $clsDefine]
  #set ::itclx::_ClassIfBuilded($className) 0
  #puts "clsBody_ext={$clsBody_ext}"
  uplevel 1 [list namespace eval "$className-" {}]
}
;

# create class(who define by class). and call "_class_extFCombo"
proc itclx::classEnd {className {fPre ""}} {
  set className [_nmspFull $className]
  _class_extFCombo $className
  uplevel 1 [list itcl::class $className $::itclx::_ClassBody($className)]
  _class_FMap $className 
}
;

# 模拟itcl::local
proc itclx::_local {class name args} {
    set ptr [uplevel [list $class $name] $args]
    uplevel [list set itcl-local-($ptr) $ptr]
    set cmd [uplevel namespace which -command $ptr]
    uplevel [list trace variable itcl-local- u \
        "::itcl::delete_helper $cmd"]
    return $ptr
}
;

proc itclx::old_New {className args} {
  variable _PublicDebug
  set className [_nmspFull $className]
  set new_pre ""
  if {[info level]>1 && $_PublicDebug==0} {set new_pre "itclx::_local "}
  set up_nmsp_pre [string map {:: /} [uplevel 1 {namespace current}]]
  if {$up_nmsp_pre=="/"} {set up_nmsp_pre ""}   ;# 当上层命名空间为全局时，后面会多加一层/
  #set objName [uplevel 1 "$new_pre$className ::$up_nmsp_pre/#auto $args"]
  set objName [uplevel 1 "$new_pre$className ::#auto $args"]
  if {$_PublicDebug!=0} {puts "  /New/ $objName"}
  return $objName
}
;

# ①用[::#auto]的方式创建对象。
# ②若在#0中执行New、或调试开关打开则创建全局对象，否则创建局部对象
proc itclx::New {className args} {
  variable _PublicDebug      ;# 引用命名空间中的变量_PublicDebug
  if {![string match {::*} $className]} {set className "::$className"}
  set new_pre ""
  if {[info level]>1 && $_PublicDebug==0} {set new_pre "itclx::_local "}
  set objName [uplevel 1 "$new_pre$className ::#auto $args"]
  if {$_PublicDebug!=0} {puts "  /New/ $objName"}
  return $objName
}

;
proc itclx::New0 {className objName args} {
  variable _PublicDebug
  set className [_nmspFull $className]
  #set objName [_nmspFull $objName]
  set new_pre ""
  if {[info level]>2 && $_PublicDebug=="0"} {set new_pre "::itcl::local "}
  #if {$::itclx::_ClassIfBuilded($className)==0} {uplevel 1 [list classEnd $className]}
  if {[uplevel 1 [list ::itcl::find objects $objName]] != ""} {
      uplevel 1 [list ::itcl::delete object $objName]
      puts "[_fUpInfo 1]/New/ ::itcl::delete object $objName"
  }
  uplevel 1 "$new_pre$className $objName $args"
  if {$_PublicDebug!="0"} {
    puts "[_fUpInfo 1]/New/ $new_pre$className [uplevel 1 {namespace current}]::$objName $args"
  }
}
;

;# 将形如[a=1 {b=1 2 } c={|3-1|} z=|3-1|]的ini格式转为形如[a 1 b {1 2 } c |3-1| z 2]的dict格式
proc itclx::ini2dict {iniList} {
  if {![string is list $iniList]} {error "错误的iniList数据: {$iniList}，须为集合形式"}
  set returnDict ""
  foreach ini $iniList {
    set matchId [string first "=" $ini]
    if {$matchId<1} {error "错误的ini数据: {$ini}，须形如{a=1 {b=1 2 } {c=}}"}
    set varEnd   [expr $matchId-1]
    set valBegin [expr $matchId+1]
    set ikey [string range $ini 0 $varEnd]
    set ivalue [string range $ini $valBegin end]
    if {[string index $ivalue 0]=="|" && [string index $ivalue end]=="|"} {
      set ivalue [expr [string range $ivalue 1 end-1]]
    } elseif {[string index $ivalue 0]=="\{" && [string index $ivalue end]=="\}"} {
      set ivalue [string range $ivalue 1 end-1]
    }
    dict set returnDict $ikey $ivalue
  }
  return $returnDict
}
;

;# 将形如[a 1 b {1 2 } c |3-1| z 2]的ini格式转为形如[a=1 {b=1 2 } c={|3-1|} z=2]
proc itclx::dict2ini {dictInfo} {
  set iniLs ""
  dict for {var val} $dictInfo {
    if {[string match {|*|} $val]} {set valNew "{$val}"} else {set valNew $val}
    lappend iniLs "$var=$valNew"
  }
  return $iniLs
}
;

# 初始化"结构体"型的对象是否初始化正常，仅建议在构造函数中使用
# 所做的操作：①以args形式接收构造函数中的值，args输入格式为ini形式，然后转为字典格式。
#               并按字典形式初始化(字典的key须为pubVar，且首先字母必须大写、且不带_后缀)
#             ②在类声明中未给定初值的为必选参数，初始化时若不带则报错
#             ③若有参数Expe，则值必须为全大写的PASS或FAIL
# (注: 所有涉及到跨函数栈执行的特殊形式，其中使用的变量名都采用特殊形式以免与上层函数栈
#      中的变量名产生冲突，每跨1层函数加1个"-"后缀)
proc itclx::Struct_Init {} {
  uplevel 1 {
    ;# 将形如[a=1 {b=1 2 }]的ini格式转为形如[a 1 b {1 2 }]的dict格式
    set args [itclx::ini2dict $args]  
    if {[llength $args]%2 != 0} {error "参数集须为字典形式"}
    set pubLs- [itclx::getPubVars $this]
    dict for {key- value-} $args {
      if {[set key-] in [set pubLs-]} {
        if {![string match {[A-Z]*[a-zA-Z0-9]} [set key-]]} {
          error "错误的对象变量名格式{[set key-]}，须首字母大写且不带_后缀，形如{Var}"
        }
        set [set key-] [set value-]
        lappend _InputKeys- [set key-]
      } else {
        error "错误的参数{[set key-]}，须属于{[set pubLs-]}"
      }
    }
    foreach key- [set pubLs-] {
      if {[catch {set [set key-]} err1-] && [catch {array get [set key-]} err2-]} {
        error "缺少必须参数{[set key-]}, err1={[set err1-]} err2={[set $err2-]}"
      }
    }
    if {"Expe" in [set pubLs-] && ([set Expe] ni "$::TSPASS $::TSFAIL")} {
      error "错误的参数Expe值={[set Expe]}，须为属于{$::TSPASS $::TSFAIL}"
    }
  }
}
;

;# 仅辅助于itclx::dim函数
proc itclx::_dim_unsetHelp {name1 name2 op} {
	set upNmsp [uplevel 1 {namespace current}]
  set level [info level]
  set unsets [info commands $upNmsp-dim-trace-($level,*)]
  #puts "_dim_unsetHelp: unsets={info commands $upNmsp-dim-trace-($level,*)}={$unsets}"
  foreach i $unsets {rename $i ""}
}
;

;# 定义属性变量的类型。 若不满足类型，则会报错(但操作还是无法阻止)
;# [一个命名空间/类中，不能有同名的dim变量]
;# 原理(除Arr/Array类型外)： trace add variable $varname write f_dim_proc
;#   f_dim_proc {varNm element opt {codes ""}} { ... } 
;# 用法： 形如 [dim Int {x0 x1}] [dim ]
proc itclx::Dim0 {args} {
	#
  set argsLen [llength $args]
  if {$argsLen==2} {
    set dimBody ""
    lassign $args varNames type
  } elseif {$argsLen==3} {
    lassign $args varNames type dimBody
  } else {
    error "错误的args长度，须为2或3"
  }
	set typeLs {Int Bool Dict IntRange InLs NiLs InClass NiClass All Custom}
	switch $type {
		Int {
			set dimEval {
				if {![string is integer $var]} {
          error "错误的$varName，需为Int型的整数。但设置值={$var}"
				}
			}
		}
		IntRange {
			lassign [split $dimBody "-"] min max
			if {![string is integer $min] || ![string is integer $max]} {
				error "错误的dimBody，需为IntRange型的范围整数，形如1-3。但设置值={$dimBody}"
			}
			set dimEval "
				if {!\[string is integer \$var] || \$var<$min || \$var>$max } {
					error \"错误的\$varName，需为$min-$min的IntRange型范围整数。但设置值={\$var}\"
				}
			"
		}
		Bool {
			set dimEval {
				if {$var ni "0 1"} {
					error "错误的变量$varName，需为Boolean型、即0或1。但设置值={$var}"
				}
			}
			;
		}
		Dict {
			set dimEval {
				if {![string is list $var] && [llength $var]%2 != 0} {
					error "错误的变量$varName，需为Dict型、即llength%2==0。但设置值={$var}"
				}
			}
			;
		}
		InLs {
      set dimBodyLower [string tolower $dimBody]
			set dimEval "
				if {!(\[string tolower \$var\] in {$dimBodyLower})} {
					error \"错误的变量\$varName，需InList型、属于集合{$dimBodyLower}。但设置值={\$var}\"
				}
			"
		}
		NiLs {
      set dimBodyLower [string tolower $dimBody]
			set dimEval "
				if {!(\[string tolower \$var\] ni {$dimBodyLower})} {
					error \"错误的变量\$varName，需NiList型、不属于集合{$dimBodyLower}。但设置值={\$var}\"
				}
			"
		}
		InClass {
      if {[itcl::find classes "::${dimBody}"]=={}} {error "dim InClass: {$dimBody}须为一个类"}
			set dimEval "
				if {\[itcl::find objects -isa $dimBody \$var\]=={}} {
					error \"错误的变量\$varName，需为类{$dimBody}的对象。但设置值={\$var}\"
				}
			"
		}
		All    { set dimEval "" }
		Custom  {
			set dimEval "
				if {!\[uplevel 1 {expr {$dimBody}}\]} {
          append err \"错误的变量\$varName，需满足表达式\"
          append err \{\{$dimBody\}\}
          append err \"。但设置值={\$var}\"
          error \$err
				}
			"
      #error \"错误的变量\$varName，需满足表达式{$dimBody}。但设置值={\$var}\"
		}
		default { error "错误的类型{$type}，需属于{$typeLs}" }
	}
	#
	
	set upNmsp [uplevel 1 {namespace current}]
  set level [expr [info level]-1]
	# 逐个执行定义变量(数组型变量是不同处理逻辑)
	foreach varName $varNames {
    if {[uplevel 1 [list info exists $varName]] == ""} {error "不存在的变量{$varName}"}
    set icmdName $upNmsp-dim-trace-($level,$varName)
    uplevel 1 [list set -dim-trace-($level,$varName) 1]
    uplevel 1 [list trace add variable -dim-trace- unset itclx::_dim_unsetHelp]
    uplevel 1 [list trace add variable $varName write $icmdName]
    
		proc $icmdName {varName element op} "
			if {\$element ne {} } { 
				set varName \${varName}(\$element) 
			} 
			upvar 1 \$varName var
			eval {$dimEval}
		"
			;# 在使用dim所在的函数栈中，初次检查一次变量。
    set up_varVal [uplevel 1 [list set $varName]]
		uplevel 1 " set $varName $up_varVal "
	}
	;
}
;#

proc itclx::Dim {args} {
	#
  set argsLen [llength $args]
  if {$argsLen==2} {
    set dimBody ""
    lassign $args varNames type
  } elseif {$argsLen==3} {
    lassign $args varNames type dimBody
  } else {
    error "错误的args长度，须为2或3"
  }
	set typeLs {Int Bool Dict IntRange InLs NiLs InClass NiClass Custom}
	switch $type {
		Int {
			set dimEval {
				if {![string is integer $__val]} {
          error "错误的$__var, 需为Int型的整数。但设置值={$__val}"
				}
			}
		}
		IntRange {
			lassign [split $dimBody "-"] min max
			if {![string is integer $min] || ![string is integer $max]} {
				error "错误的dimBody，需为IntRange型的范围整数，形如1-3。但设置值={$dimBody}"
			}
			set dimEval "
				if {!(\[string is integer \$__val] && \$__val<=$max && \$__val>=$min)} {
					error \"错误的\$__var, 需为$min-$max的IntRange型范围整数。但设置值={\$__val}\"
				}
			"
		}
		Bool {
			set dimEval {
				if {$__val ni "0 1"} {
					error "错误的变量$__var, 需为Boolean型、即0或1。但设置值={$__val}"
				}
			}
			;
		}
		Dict {
			set dimEval {
				if {![string is list $__val] && [llength $__val]%2 != 0} {
					error "错误的变量$__var, 需为Dict型、即llength%2==0。但设置值={$__val}"
				}
			}
			;
		}
		InLs {
      set dimBodyLower [string tolower $dimBody]
			set dimEval "
				if {!(\[string tolower \$__val\] in {$dimBodyLower})} {
					error \"错误的变量\$__var, 需InList型、属于集合{$dimBodyLower}。但设置值={\$__val}\"
				}
			"
		}
		NiLs {
      set dimBodyLower [string tolower $dimBody]
			set dimEval "
				if {!(\[string tolower \$__val\] ni {$dimBodyLower})} {
					error \"错误的变量\$__var, 需NiList型、不属于集合{$dimBodyLower}。但设置值={\$__val}\"
				}
			"
		}
		InClass {
      if {[itcl::find classes "::${dimBody}"]=={}} {error "dim InClass: {$dimBody}须为一个类"}
			set dimEval "
				if {\[itcl::find objects -isa $dimBody \$__val\]=={}} {
					error \"错误的变量\$__var, 需为类{$dimBody}的对象。但设置值={\$__val}\"
				}
			"
		}
		Custom  {
			set dimEval "
				if {$dimBody} {} else {
          append err \"错误的变量\$__var, 需满足表达式\"
          append err \{\{$dimBody\}\}
          append err \"。但设置值={\$__val}\"
          error \$err
				}
			"
      #error \"错误的变量\$varName，需满足表达式{$dimBody}。但设置值={\$var}\"
		}
		default { error "错误的类型{$type}，需属于{$typeLs}" }
	}
	#
	
	foreach varName $varNames {
    uplevel 1 "set __var $varName; set __val \$$varName"
		uplevel 1 $dimEval
	}
	;
}
;


# "itclx::class $className fName" => auto map to "$className-::$fName"
proc itclx::_class_FMap {className} {
  set className [_nmspFull $className]
	foreach fFull [info commands "$className-::*"] {
    set f [namespace tail $fFull]
		set fFull_body "eval \[::info body $fFull\]"
		set fFull_argsNames [info args $fFull]
		set fFull_argsLs ""
		foreach fFull_arg $fFull_argsNames {
			if [info default $fFull $fFull_arg fFull_def] {
				lappend fFull_argsLs [list $fFull_arg $fFull_def]
			} else {
				lappend fFull_argsLs $fFull_arg
			}
		}
		uplevel #0 "itcl::body ${className}::$f \{$fFull_argsLs\} {$fFull_body}"		
	}
}
;

# extend class
# pubMethod:=> reg{^[A-Z].*_[A-Z].*[^_]$} or {^[A-Z][^_]*[^_]$}
# proMethod:=> reg{^[A-Z].*_[a-z].*[^_]$} or {^[a-z].*[^_]$}
proc itclx::_class_extFCombo {className} {
  set className [_nmspFull $className]
  set clsBody "" ;# $::itclx::_ClassBody($className)
  
    # proM: 小写开头、不以_结尾
  set proMethods [lsort -dictionary [info procs "$className-::\[a-z\]*\[0-9a-zA-Z?\]"]]  
  set pubMethods ""
    # 大写开头、不以_结尾
  foreach iMethod [lsort -dictionary [info procs "$className-::\[A-Z\]*\[0-9a-zA-Z?\]"]] {
    set iMethod [namespace tail $iMethod]
    if {![string match {*_*} $iMethod]} {
      lappend pubMethods $iMethod     ;# pubM: 大写开头、中间不带_、不以_结尾
    } else {
      if {[string match {*_[A-Z?]*} $iMethod]} {
        lappend pubMethods $iMethod   ;# pubM: 大写开头、中间带_大写开头、不以_结尾
      } else {
        lappend proMethods $iMethod   ;# proM: 大写开头、中间带_小写开头、不以_结尾
      }
    }
  }
  
  # 由于当前底层的itcl包未重写，故暂时用pub级别代替pro级别
  set proLv public   ;# protected
  foreach proMethod $proMethods {
    append clsBody "\n$proLv method [namespace tail $proMethod]"
  }
  foreach pubMethod $pubMethods {
    append clsBody "\npublic method [namespace tail $pubMethod]"
  }
  foreach proProc [lsort [info procs "$className-::\[a-z\]*_"]] {
    append clsBody "\n$proLv proc [namespace tail $proProc]"
  }
  foreach pubProc [lsort [info procs "$className-::\[A-Z\]*_"]] {
    append clsBody "\npublic proc [namespace tail $pubProc]"
  }
  set ::itclx::_ClassBody($className) "$clsBody\n$::itclx::_ClassBody($className)"
}
;

# 自动扩展classBody 
# Eval-：无限制的内部调试执行，不应该在实际代码中出现、只用于内部调试
# Get-:  获取任意属性的值(自动判定是否为数组变量)
# Set-:  对public级variable进行设置。(如需设置类变量，应该仅能通过方法去设置)
proc itclx::_class_extBody {className clsBody} {
  set className [_nmspFull $className]
  append clsBody "\n"
  append clsBody {
    public variable _InputKeys- ""
    public method Get- {__var} {
      if [::array exists $__var] {::array get $__var} else {::set $__var}
    }
    public method Set- {__var __val} {
      set pubVars [itclx::getPubVars $this]
      if {$__var ni $pubVars} {error "unknown public variable {$__var}, must in {$pubVars}"}
      
      if [::array exists $__var] {::array set $__var $__val} else {::set $__var $__val}
      eval [$this info variable $__var -config]   ;# 执行pubVar的config部分
      if [::array exists $__var] {::array get $__var} else {::set $__var}
    }
    public method Eval- {__script} {
      ::eval $__script
    }
  }
  #append clsBody [::itclx::defineVar_extBody $className]
  return $clsBody
}
;


proc itclx::_nmspFull {nmsp} {
  if {![string match {::*} $nmsp]} {set nmsp "::$nmsp"}
  return $nmsp
}
;

# 查看上级函数调用时信息。 ::info加前缀::防止与[$obj info]冲突
proc itclx::_fUpInfo {{upLayer 0}} {
	set level [expr [::info level]-1-$upLayer]
  #if {$level<0} {return "bad level $level"}
	if {$level<=0} {
		set levelInfo " #0 "
	} else {
		set levelInfo [::info level $level]
	}
	return "[string repeat {  } $level]#$level\[$levelInfo]: "
}
;#

;# 
proc itclx::Here {} {
	set level [expr [info level]-2]
  #if {$level<0} {return "bad level $level"}
	if {$level<=0} {
		set levelInfo " #0 "
	} else {
		set levelInfo [info level $level]
	}
	return "[string repeat {  } $level]#$level\[$levelInfo]: "
  itclx::_fUpInfo 1
}
;

;# itcl::class User {...} ; itcl::class Movie { common x [User #auto] }
proc itclx::_new {args} {
  set namespace "::"
  set arg0 [lindex $args 0]
  if [string match $arg0 {-*}] {
    set namespace [string range $arg0 1 end]
    set className [lindex $args 1]
    set args [lrange $args 2 end]
  } else {
    set className $arg0
    set args [lrange $args 1 end]
  }
  #
  return [$className "$namespace#auto" {*}$args]
}
;



set itclx::TEST_oo {
  class Test1_oo {
    public common ObjName ""
    public method Eval cmd {eval $cmd}
  }
  classEnd Test1_oo
  class Test2_oo {
    public variable var 0
    protected variable var2 var2
    public method f {args} {return "f {$args}"}
    protected method f2 {args} {return "f {$args}"}
    constructor {args} {set Test1_oo::ObjName $this; puts "[itclx::_fUpInfo 0]: args={$args}"}
  }
  classEnd Test2_oo
  set t1 [New Test1_oo]
  set t2 [New Test2_oo]
  #puts "$Test1_oo::ObjName"
  set testInfo "
    {.$t2.var = 1} 1  {.$t2.var} 1   {catch {.$t2.var2 = 1}} 1  {catch {.$t2.var2}} 0
    {.$t1.ObjName.var} 1  {.$t1.ObjName.var = 2} 2  {.$t1.ObjName.var} 2
    {.$t1.ObjName.f}  {f {}}   {.$t1.ObjName.f 1}  {f {1}}  {.$t1.ObjName.f 1 2}  {f {1 2}}
    {.$t2.f}  {f {}}   {.$t2.f 1}  {f {1}}  {.$t2.f 1 2}  {f {1 2}}
    {catch {.$t2.f2}} 1
  "
  foreach {itest iexpect} $testInfo {
    set iout [eval $itest]
    if {$iout!=$iexpect} {error "{$iout}!={$iexpect} , {$itest}"}
  }
  proc itclx::test_oo {cmd} {eval $cmd}
  foreach {itest iexpect} $testInfo {
    set iout [itclx::test_oo $itest]
    if {$iout!=$iexpect} {error "{$iout}!={$iexpect} , {$itest}"}
  }
  puts "TEST_oo success"
}
# ex0.1: [oo var]==[itclx::_get var];    ex0.2: [oo var = val]==[itclx::_set var val]
# ex1: [oo obj1.objN.var]==[[obj1 Get- objN] Get- var]
# ex2: [oo obj1.objN.var = val]==[[obj1 Get- objN] configure -var val]
# ex3: [oo obj1.objN.var ...]==[[obj1 Get- objN] configure -var val]
# 别用oo执行函数，会使得函数栈变得混乱. (例外：_PublicDebug模式下，全在#0栈调试，不担心栈混乱)
proc itclx::oo {args} {
  set argLen [llength $args]
  lassign $args __namePath __input __val    ;# ex2: obj1.objN.var = val
  set nameLs [split $__namePath "."]        ;# ex2: {obj1 objN var}
  set nameNum [llength $nameLs]             ;# ex2: 3
  
  # 类的内部相互调用。在全局调试时配合::this变量很有用(方法内的代码可以直接进行调试)
  if {$nameNum==1} {
    set __obj [set thisUp [uplevel 1 "set this"]]
    set __nameEnd $nameLs
    if {$__nameEnd in [itclx::getMethods $__obj]} {
      set levelUp 1
      #set levelUp [expr min(2,[::info level])]
        # 【待完善】PublicDebug调试开关打开时，所有的itclx面向对象权限都改为pub。  否则在#0调试模式下无法调用非pub函数。
      
        # 通过Eval-Debug-间接执行时，可会多占用1层函数栈
      return [uplevel $levelUp "{$__obj} $args"]
    } elseif {$argLen==1} { # ex0.1: [oo var]==[itclx::_get var]
      return [$thisUp Get- $nameLs]
    } elseif {$argLen==3 && $__input=="="} { #ex0.2: [oo var = val]==[itclx::_set var val]
      return [$thisUp Eval- "if \[array exists {$nameLs}] \
        {array set {$nameLs} {$__val}} else {set {$nameLs} {$__val}} \
      "]
    } else {
      error "itclx::oo $args"
    }
  }
 
  set __obj [itclx::_nmspFull [lindex $nameLs 0]]
  for {set i 1} {$i<$nameNum-1} {incr i} {
    set __obj [itclx::_nmspFull [$__obj Get- [lindex $nameLs $i]]]
  }
  set __nameEnd [lindex $nameLs end]
  
  #if {$::itclx::_PublicDebug} {puts "argLen,__obj,__nameEnd: {$argLen} {$__obj} {$__nameEnd}"}
  if {$__nameEnd in [itclx::getMethods $__obj]} {
    set ret [uplevel 1 "{$__obj} {$__nameEnd} [lrange $args 1 end]" ]
  } elseif {$argLen==1} {
    set ret [$__obj Get- $__nameEnd]
  } elseif {$argLen==3 && $__input=="="} {
    set ret [$__obj Set- $__nameEnd $__val]
  } else {
    error "itclx::oo $args"
  }
  return $ret
}
;
proc itclx::oo_old {args} {
  set argLen [llength $args]
  lassign $args __namePath __input __val    ;# ex2: obj1.objN.var = val
  set nameLs [split $__namePath "."]        ;# ex2: {obj1 objN var}
  set nameNum [llength $nameLs]             ;# ex2: 3
  
  # 类的内部相互调用。在全局调试时配合::this变量很有用(方法内的代码可以直接进行调试)
  if {$nameNum==1} {
    set __obj [set thisUp [uplevel 1 "set this"]]
    set __nameEnd $nameLs
    if {$__nameEnd in [itclx::getMethods $__obj]} {
      set levelUp [expr min(2,[::info level])]
        # 通过Eval-Debug-间接执行时，可会多占用1层函数栈
      return [uplevel $levelUp [list $__obj Eval- $args]]
    } elseif {$argLen==1} { # ex0.1: [oo var]==[itclx::_get var]
      return [$thisUp Get- $nameLs]
    } elseif {$argLen==3 && $__input=="="} { #ex0.2: [oo var = val]==[itclx::_set var val]
      return [$thisUp Eval- "if \[array exists {$nameLs}] \
        {array set {$nameLs} {$__val}} else {set {$nameLs} {$__val}} \
      "]
    } else {
      error "itclx::oo $args"
    }
  }

  set __obj [itclx::_nmspFull [lindex $nameLs 0]]
  for {set i 1} {$i<$nameNum-1} {incr i} {
    set __obj [itclx::_nmspFull [$__obj Get- [lindex $nameLs $i]]]
  }
  set __nameEnd [lindex $nameLs end]
  
  #if {$::itclx::_PublicDebug} {puts "argLen,__obj,__nameEnd: {$argLen} {$__obj} {$__nameEnd}"}
  if {$__nameEnd in [itclx::getMethods $__obj]} {
    set ret [uplevel 1 "{$__obj} {$__nameEnd} [lrange $args 1 end]" ]
  } elseif {$argLen==1} {
    set ret [$__obj Get- $__nameEnd]
  } elseif {$argLen==3 && $__input=="="} {
    set ret [$__obj Set- $__nameEnd $__val]
  } else {
    error "itclx::oo $args"
  }
  return $ret
}
;

proc itclx::getPubVars {obj} {
  set pubVars ""
  foreach i [uplevel 1 [list $obj configure]] {
    lappend pubVars [string range [lindex $i 0] 1 end]
  }
  return $pubVars
}
;

# 获取对象中的所有公开级对象方法(不包括类方法)
proc itclx::getMethods {obj {sort 1}} {
  set obj [itclx::_nmspFull $obj]
  set methods ""
  foreach i [$obj info function] {
    set j [namespace tail $i]
    if {$j ni $methods} {lappend methods $j}
  }
  if {$sort} {set methods [lsort -dictionary $methods]}
  return $methods
}
;

# 获取对象中的可用方法(不只是public级别的)
proc itclx::getFuncLs {obj {sort 1}} {
  set obj [itclx::_nmspFull $obj]
  #if {[info exists ::itclx::_ClassFuncLs($obj)]} {return $::itclx::_ClassFuncLs($obj)}
  
  set errInfoOld $::errorInfo
  catch "$obj <undefined-function>" err
  set ::errorInfo $errInfoOld
  if {[string match {invalid command name *} $err]} {error $err}
  set fLs ""
  if [string match "unknown method*" $err] {
    set startAt [string first ": must be" $err]
  }
  foreach i [lrange [split $err "\n"] 1 end] {
    lappend fLs [lindex $i 1]
  }
  if {$sort} {set fLs [lsort -dictionary $fLs]}
  return $fLs
}
;

proc itclx::exsits {obj} {
  foreach i [$obj configure] {
    lappend pubVars [string range [lindex $i 0] 1 end]
  }
  return $pubVars
}
;

proc itclx::_findObj {objName} {
  set objResult [uplevel 1 "itcl::find objects $objName"]
  if {$objResult==""} {
    set objName [uplevel 1 "itclx::_nmspFull $objName"]
    set objResult [uplevel 1 "itcl::find objects $objName"]
  }
  return $objResult
}
;
proc itclx::find {args} {
  return [uplevel #0 "::itcl::find {*}$args"]
}
;

proc itclx::Lassign {lsInfo obj varLs} {
  foreach var $varLs val $lsInfo {
    if {$var == ""} {continue}
    .$obj.$var = $val
  }
}
;

;# 若定义A类时使用了[MsgTrust "B C"]，在B/C类中可通过[MsgSend a ...]访问类A实例a的_MsgReceive-接口
proc itclx::MsgTrust {classList} {
  uplevel 1 "protected common _MsgTrustClassLs_- {$classList}"
  uplevel 1 "protected method _MsgReceive- {msg} {eval \$msg}" ;# _MsgReceive-为抽象消息接收接口
}
;

;# 若定义A类时使用了[MsgTrust "B C"]，在B/C类中可通过[MsgSend a ...]访问类A实例a的_MsgReceive-接口
proc itclx::MsgSend {dstObj msg} {
  set srcHeritageLs [uplevel 1 {$this info heritage}]
  set dstTrustLs [.$dstObj._MsgTrustClassLs_-]
  foreach dstTrust $dstTrustLs {
    if {[itclx::_nmspFull $dstTrust] in $srcHeritageLs} {
      return [.$dstObj.Eval- "_MsgReceive- {$msg}"]
    }
  }
  error "dstTrustLs={$dstTrustLs} ni srcHeritageLs={$srcHeritageLs}"
}
;

proc ---NMSP---END {} {}
namespace import ::itclx::*
eval $itclx::TEST_oo
package provide itclx 2.0









