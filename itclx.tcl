# pkg_mkIndex -direct "lib/package/itclx" *.tcl
puts "itclx.tcl Initialed. when \[itclx::publicDebug on] , all oo operate will public" 

package require itcl


proc ---NMSP---itclx {} {}
namespace eval itclx {
  namespace export class classEnd oo New MsgTrust MsgSend Struct_Init Dim Lassign
  variable _PublicDebug 0   ;# ��ֵ��Ϊ0ʱ������ͨ��oo�Ĳ�����ΪpublicȨ��
}
;

;# ����_PublicDebug����
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

# ģ��itcl::local
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
  if {$up_nmsp_pre=="/"} {set up_nmsp_pre ""}   ;# ���ϲ������ռ�Ϊȫ��ʱ���������һ��/
  #set objName [uplevel 1 "$new_pre$className ::$up_nmsp_pre/#auto $args"]
  set objName [uplevel 1 "$new_pre$className ::#auto $args"]
  if {$_PublicDebug!=0} {puts "  /New/ $objName"}
  return $objName
}
;

# ����[::#auto]�ķ�ʽ��������
# ������#0��ִ��New������Կ��ش��򴴽�ȫ�ֶ��󣬷��򴴽��ֲ�����
proc itclx::New {className args} {
  variable _PublicDebug      ;# ���������ռ��еı���_PublicDebug
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

;# ������[a=1 {b=1 2 } c={|3-1|} z=|3-1|]��ini��ʽתΪ����[a 1 b {1 2 } c |3-1| z 2]��dict��ʽ
proc itclx::ini2dict {iniList} {
  if {![string is list $iniList]} {error "�����iniList����: {$iniList}����Ϊ������ʽ"}
  set returnDict ""
  foreach ini $iniList {
    set matchId [string first "=" $ini]
    if {$matchId<1} {error "�����ini����: {$ini}��������{a=1 {b=1 2 } {c=}}"}
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

;# ������[a 1 b {1 2 } c |3-1| z 2]��ini��ʽתΪ����[a=1 {b=1 2 } c={|3-1|} z=2]
proc itclx::dict2ini {dictInfo} {
  set iniLs ""
  dict for {var val} $dictInfo {
    if {[string match {|*|} $val]} {set valNew "{$val}"} else {set valNew $val}
    lappend iniLs "$var=$valNew"
  }
  return $iniLs
}
;

# ��ʼ��"�ṹ��"�͵Ķ����Ƿ��ʼ���������������ڹ��캯����ʹ��
# �����Ĳ���������args��ʽ���չ��캯���е�ֵ��args�����ʽΪini��ʽ��Ȼ��תΪ�ֵ��ʽ��
#               �����ֵ���ʽ��ʼ��(�ֵ��key��ΪpubVar����������ĸ�����д���Ҳ���_��׺)
#             ������������δ������ֵ��Ϊ��ѡ��������ʼ��ʱ�������򱨴�
#             �����в���Expe����ֵ����Ϊȫ��д��PASS��FAIL
# (ע: �����漰���纯��ջִ�е�������ʽ������ʹ�õı�����������������ʽ�������ϲ㺯��ջ
#      �еı�����������ͻ��ÿ��1�㺯����1��"-"��׺)
proc itclx::Struct_Init {} {
  uplevel 1 {
    ;# ������[a=1 {b=1 2 }]��ini��ʽתΪ����[a 1 b {1 2 }]��dict��ʽ
    set args [itclx::ini2dict $args]  
    if {[llength $args]%2 != 0} {error "��������Ϊ�ֵ���ʽ"}
    set pubLs- [itclx::getPubVars $this]
    dict for {key- value-} $args {
      if {[set key-] in [set pubLs-]} {
        if {![string match {[A-Z]*[a-zA-Z0-9]} [set key-]]} {
          error "����Ķ����������ʽ{[set key-]}��������ĸ��д�Ҳ���_��׺������{Var}"
        }
        set [set key-] [set value-]
        lappend _InputKeys- [set key-]
      } else {
        error "����Ĳ���{[set key-]}��������{[set pubLs-]}"
      }
    }
    foreach key- [set pubLs-] {
      if {[catch {set [set key-]} err1-] && [catch {array get [set key-]} err2-]} {
        error "ȱ�ٱ������{[set key-]}, err1={[set err1-]} err2={[set $err2-]}"
      }
    }
    if {"Expe" in [set pubLs-] && ([set Expe] ni "$::TSPASS $::TSFAIL")} {
      error "����Ĳ���Expeֵ={[set Expe]}����Ϊ����{$::TSPASS $::TSFAIL}"
    }
  }
}
;

;# ��������itclx::dim����
proc itclx::_dim_unsetHelp {name1 name2 op} {
	set upNmsp [uplevel 1 {namespace current}]
  set level [info level]
  set unsets [info commands $upNmsp-dim-trace-($level,*)]
  #puts "_dim_unsetHelp: unsets={info commands $upNmsp-dim-trace-($level,*)}={$unsets}"
  foreach i $unsets {rename $i ""}
}
;

;# �������Ա��������͡� �����������ͣ���ᱨ��(�����������޷���ֹ)
;# [һ�������ռ�/���У�������ͬ����dim����]
;# ԭ��(��Arr/Array������)�� trace add variable $varname write f_dim_proc
;#   f_dim_proc {varNm element opt {codes ""}} { ... } 
;# �÷��� ���� [dim Int {x0 x1}] [dim ]
proc itclx::Dim0 {args} {
	#
  set argsLen [llength $args]
  if {$argsLen==2} {
    set dimBody ""
    lassign $args varNames type
  } elseif {$argsLen==3} {
    lassign $args varNames type dimBody
  } else {
    error "�����args���ȣ���Ϊ2��3"
  }
	set typeLs {Int Bool Dict IntRange InLs NiLs InClass NiClass All Custom}
	switch $type {
		Int {
			set dimEval {
				if {![string is integer $var]} {
          error "�����$varName����ΪInt�͵�������������ֵ={$var}"
				}
			}
		}
		IntRange {
			lassign [split $dimBody "-"] min max
			if {![string is integer $min] || ![string is integer $max]} {
				error "�����dimBody����ΪIntRange�͵ķ�Χ����������1-3��������ֵ={$dimBody}"
			}
			set dimEval "
				if {!\[string is integer \$var] || \$var<$min || \$var>$max } {
					error \"�����\$varName����Ϊ$min-$min��IntRange�ͷ�Χ������������ֵ={\$var}\"
				}
			"
		}
		Bool {
			set dimEval {
				if {$var ni "0 1"} {
					error "����ı���$varName����ΪBoolean�͡���0��1��������ֵ={$var}"
				}
			}
			;
		}
		Dict {
			set dimEval {
				if {![string is list $var] && [llength $var]%2 != 0} {
					error "����ı���$varName����ΪDict�͡���llength%2==0��������ֵ={$var}"
				}
			}
			;
		}
		InLs {
      set dimBodyLower [string tolower $dimBody]
			set dimEval "
				if {!(\[string tolower \$var\] in {$dimBodyLower})} {
					error \"����ı���\$varName����InList�͡����ڼ���{$dimBodyLower}��������ֵ={\$var}\"
				}
			"
		}
		NiLs {
      set dimBodyLower [string tolower $dimBody]
			set dimEval "
				if {!(\[string tolower \$var\] ni {$dimBodyLower})} {
					error \"����ı���\$varName����NiList�͡������ڼ���{$dimBodyLower}��������ֵ={\$var}\"
				}
			"
		}
		InClass {
      if {[itcl::find classes "::${dimBody}"]=={}} {error "dim InClass: {$dimBody}��Ϊһ����"}
			set dimEval "
				if {\[itcl::find objects -isa $dimBody \$var\]=={}} {
					error \"����ı���\$varName����Ϊ��{$dimBody}�Ķ��󡣵�����ֵ={\$var}\"
				}
			"
		}
		All    { set dimEval "" }
		Custom  {
			set dimEval "
				if {!\[uplevel 1 {expr {$dimBody}}\]} {
          append err \"����ı���\$varName����������ʽ\"
          append err \{\{$dimBody\}\}
          append err \"��������ֵ={\$var}\"
          error \$err
				}
			"
      #error \"����ı���\$varName����������ʽ{$dimBody}��������ֵ={\$var}\"
		}
		default { error "���������{$type}��������{$typeLs}" }
	}
	#
	
	set upNmsp [uplevel 1 {namespace current}]
  set level [expr [info level]-1]
	# ���ִ�ж������(�����ͱ����ǲ�ͬ�����߼�)
	foreach varName $varNames {
    if {[uplevel 1 [list info exists $varName]] == ""} {error "�����ڵı���{$varName}"}
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
			;# ��ʹ��dim���ڵĺ���ջ�У����μ��һ�α�����
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
    error "�����args���ȣ���Ϊ2��3"
  }
	set typeLs {Int Bool Dict IntRange InLs NiLs InClass NiClass Custom}
	switch $type {
		Int {
			set dimEval {
				if {![string is integer $__val]} {
          error "�����$__var, ��ΪInt�͵�������������ֵ={$__val}"
				}
			}
		}
		IntRange {
			lassign [split $dimBody "-"] min max
			if {![string is integer $min] || ![string is integer $max]} {
				error "�����dimBody����ΪIntRange�͵ķ�Χ����������1-3��������ֵ={$dimBody}"
			}
			set dimEval "
				if {!(\[string is integer \$__val] && \$__val<=$max && \$__val>=$min)} {
					error \"�����\$__var, ��Ϊ$min-$max��IntRange�ͷ�Χ������������ֵ={\$__val}\"
				}
			"
		}
		Bool {
			set dimEval {
				if {$__val ni "0 1"} {
					error "����ı���$__var, ��ΪBoolean�͡���0��1��������ֵ={$__val}"
				}
			}
			;
		}
		Dict {
			set dimEval {
				if {![string is list $__val] && [llength $__val]%2 != 0} {
					error "����ı���$__var, ��ΪDict�͡���llength%2==0��������ֵ={$__val}"
				}
			}
			;
		}
		InLs {
      set dimBodyLower [string tolower $dimBody]
			set dimEval "
				if {!(\[string tolower \$__val\] in {$dimBodyLower})} {
					error \"����ı���\$__var, ��InList�͡����ڼ���{$dimBodyLower}��������ֵ={\$__val}\"
				}
			"
		}
		NiLs {
      set dimBodyLower [string tolower $dimBody]
			set dimEval "
				if {!(\[string tolower \$__val\] ni {$dimBodyLower})} {
					error \"����ı���\$__var, ��NiList�͡������ڼ���{$dimBodyLower}��������ֵ={\$__val}\"
				}
			"
		}
		InClass {
      if {[itcl::find classes "::${dimBody}"]=={}} {error "dim InClass: {$dimBody}��Ϊһ����"}
			set dimEval "
				if {\[itcl::find objects -isa $dimBody \$__val\]=={}} {
					error \"����ı���\$__var, ��Ϊ��{$dimBody}�Ķ��󡣵�����ֵ={\$__val}\"
				}
			"
		}
		Custom  {
			set dimEval "
				if {$dimBody} {} else {
          append err \"����ı���\$__var, ��������ʽ\"
          append err \{\{$dimBody\}\}
          append err \"��������ֵ={\$__val}\"
          error \$err
				}
			"
      #error \"����ı���\$varName����������ʽ{$dimBody}��������ֵ={\$var}\"
		}
		default { error "���������{$type}��������{$typeLs}" }
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
  
    # proM: Сд��ͷ������_��β
  set proMethods [lsort -dictionary [info procs "$className-::\[a-z\]*\[0-9a-zA-Z?\]"]]  
  set pubMethods ""
    # ��д��ͷ������_��β
  foreach iMethod [lsort -dictionary [info procs "$className-::\[A-Z\]*\[0-9a-zA-Z?\]"]] {
    set iMethod [namespace tail $iMethod]
    if {![string match {*_*} $iMethod]} {
      lappend pubMethods $iMethod     ;# pubM: ��д��ͷ���м䲻��_������_��β
    } else {
      if {[string match {*_[A-Z?]*} $iMethod]} {
        lappend pubMethods $iMethod   ;# pubM: ��д��ͷ���м��_��д��ͷ������_��β
      } else {
        lappend proMethods $iMethod   ;# proM: ��д��ͷ���м��_Сд��ͷ������_��β
      }
    }
  }
  
  # ���ڵ�ǰ�ײ��itcl��δ��д������ʱ��pub�������pro����
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

# �Զ���չclassBody 
# Eval-�������Ƶ��ڲ�����ִ�У���Ӧ����ʵ�ʴ����г��֡�ֻ�����ڲ�����
# Get-:  ��ȡ�������Ե�ֵ(�Զ��ж��Ƿ�Ϊ�������)
# Set-:  ��public��variable�������á�(���������������Ӧ�ý���ͨ������ȥ����)
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
      eval [$this info variable $__var -config]   ;# ִ��pubVar��config����
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

# �鿴�ϼ���������ʱ��Ϣ�� ::info��ǰ׺::��ֹ��[$obj info]��ͻ
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
# ����ooִ�к�������ʹ�ú���ջ��û���. (���⣺_PublicDebugģʽ�£�ȫ��#0ջ���ԣ�������ջ����)
proc itclx::oo {args} {
  set argLen [llength $args]
  lassign $args __namePath __input __val    ;# ex2: obj1.objN.var = val
  set nameLs [split $__namePath "."]        ;# ex2: {obj1 objN var}
  set nameNum [llength $nameLs]             ;# ex2: 3
  
  # ����ڲ��໥���á���ȫ�ֵ���ʱ���::this����������(�����ڵĴ������ֱ�ӽ��е���)
  if {$nameNum==1} {
    set __obj [set thisUp [uplevel 1 "set this"]]
    set __nameEnd $nameLs
    if {$__nameEnd in [itclx::getMethods $__obj]} {
      set levelUp 1
      #set levelUp [expr min(2,[::info level])]
        # �������ơ�PublicDebug���Կ��ش�ʱ�����е�itclx�������Ȩ�޶���Ϊpub��  ������#0����ģʽ���޷����÷�pub������
      
        # ͨ��Eval-Debug-���ִ��ʱ���ɻ��ռ��1�㺯��ջ
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
  
  # ����ڲ��໥���á���ȫ�ֵ���ʱ���::this����������(�����ڵĴ������ֱ�ӽ��е���)
  if {$nameNum==1} {
    set __obj [set thisUp [uplevel 1 "set this"]]
    set __nameEnd $nameLs
    if {$__nameEnd in [itclx::getMethods $__obj]} {
      set levelUp [expr min(2,[::info level])]
        # ͨ��Eval-Debug-���ִ��ʱ���ɻ��ռ��1�㺯��ջ
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

# ��ȡ�����е����й��������󷽷�(�������෽��)
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

# ��ȡ�����еĿ��÷���(��ֻ��public�����)
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

;# ������A��ʱʹ����[MsgTrust "B C"]����B/C���п�ͨ��[MsgSend a ...]������Aʵ��a��_MsgReceive-�ӿ�
proc itclx::MsgTrust {classList} {
  uplevel 1 "protected common _MsgTrustClassLs_- {$classList}"
  uplevel 1 "protected method _MsgReceive- {msg} {eval \$msg}" ;# _MsgReceive-Ϊ������Ϣ���սӿ�
}
;

;# ������A��ʱʹ����[MsgTrust "B C"]����B/C���п�ͨ��[MsgSend a ...]������Aʵ��a��_MsgReceive-�ӿ�
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









