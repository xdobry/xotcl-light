# This is XOTcl implementation in pure Tcl on top of TclOO
# tcl see http://tcl.tk
# XOTcl see http://xotcl.org
# Author Tcl programming http://www.xdobry.de/tclcorner
# written by Artur Trzewik 01/2015 mail@xdobry.de
# MIT Licence

package require Tcl 8.6
package require TclOO

package provide XOTclLight 0.1
package provide XOTcl 1.6


namespace eval ::xotcl {
	set ::xotcl::version 1.9
	set ::xotcl::patchlevel .0

	proc next {args} {
		try {
			if {[llength $args]==0} {
				set linfo [info level -1]
				if {[lindex $linfo 0] eq "::oo::Helpers::next"} {
					uplevel [list ::oo::Helpers::next {*}[lrange $linfo 1 end]]
				} else {
					uplevel [list ::oo::Helpers::next {*}[lrange $linfo 2 end]]
				}
			} else {
				uplevel [list ::oo::Helpers::next {*}$args]
			}
		} trap {TCL OO NOTHING_NEXT} {} {
			# just ignore is no active next call possible (like XOTcl do)
		}
	}

	proc _autoname {name} {
		variable _autoarr
		if {![info exists _autoarr($name)]} {
			set _autoarr($name) 0
		}
		return "$name[incr _autoarr($name)]"
	}
	
	proc _checkArguments {args min max description} {
		set len [llength $args]
		if {$len<$min || ($max>0 && $len>$max)} {
			error "expected method argumets for $description but is [info level -1]"
		}
	}
	
	proc _filterList {arguments globIndex list} {
		if {[llength $arguments]<=$globIndex} {
			set ret [list]
			set glob [lindex $arguments $globIndex]
			foreach e $list {
				if {[string match $e $glob]} {
					lappend ret $e
				}
			}
			if {[llength $ret]>0} {
				return $ret
			}
			return
		} else {
			return $list
		}
	}
	
	proc _getObjName name {
		if {[string range $name 0 1] ne "::"} {
			return ::$name
		} else {
			return $name
		}
	}
	
	proc _delete_helper {obj args} {
		$obj destroy
	}

  ::oo::class create Object {
	self {
		method unknown args {
			uplevel Object create $args
		}
		method create {name args} {
			set name [::xotcl::_getObjName $name]
			set inst [my createWithNamespace $name $name {*}$args]
			return $inst
		}
	}
	method proc {name params body} {
		oo::objdefine [self object] method $name $params $body
	}
	method instvar {args} {
		foreach var $args {
			uplevel my variable $var
		}
	}
	method init {args} {
		# empty
	}
	method set {var args} {
		set [info object namespace [self]]::$var {*}$args
	}
	method unset {args} {
		my eval unset {*}$args
	}
	method exists {var} {
		info exists [info object namespace [self]]::$var
	}
	method incr {var args} {
		incr [info object namespace [self]]::$var {*}$args
	}
	method lappend {var args} {
		lappend [info object namespace [self]]::$var {*}$args
	}
	method array {opt var args} {
		array $opt [info object namespace [self]]::$var {*}$args
	}
	method mixin {args} {
		if {[llength $args]>1} {
			switch [lindex $args 0] {
			assign {
				oo::objdefine mixin [self object] -set {*}[lrange $args 1 end]
			}
			get {
				return [info object mixin [self object]]
			}
			add {
				oo::objdefine mixin -append {*}[lrange $args 1 end]
			}
			delete {
				set cmixins [info object mixin [self object]]
				set todelete [lindex $args 1]
				set i [lsearch $cmixins $todelete]
				if {$i>=0} {
					set cmixins [lreplace $cmixins $i $i]
					oo::objdefine mixin [self object] -set {*}$cmixins
				}
			}
			default {
				oo::objdefine mixin [self object] {*}$args
			}
			}
		} elseif {[llength $args]==0} {
			info object mixin [self object]
		} else {
			oo::objdefine mixin [self object] {*}$args
		}
	}
	# first init parameters then configure slashed method "-method value" or [list -method value]
	method configure {args} {
		set len [llength $args]
		set i 0
		for {} {$i<$len} {incr i} {
			set a [lindex $args $i]
			if {[string index $a 0] eq "-" && ![string is integer [string index $a 1]]} {
				break;
			}
		}
		set skipArgs $i
		for {} {$i<$len} {incr i} {
			set a [lindex $args $i]
			if {[llength $a]==2} {
				set method [lindex $a 0]
				set param [lindex $a 1]
				if {[string index $method 0] ne "-"} {
					error "expect dashed method for configure -method ..."
				}
				my [string range $method 1 end] $param
			} else {
				set method $a
				if {[string index $method 0] ne "-"} {
					error "expect dashed method for configure -method ..."
				}
				if {$i<$len-1} {
					set param [lindex $args [expr {$i+1}]]
					if {[string index $param 0] ne "-"} {
						my [string range $method 1 end] $param
						incr i
						continue
					}
				}
				my [string range $method 1 end]
			}
		}
		return $skipArgs
	}
	method childof {obj} {
		# only dummy for configure is used by new
	}
	method volatile {} {
		set ptr [namespace tail [self object]]
		# potential problem, how to get rigth level, 2 works for regular case
		set level 2
		uplevel $level [list set itcl-local-$ptr $ptr]
		uplevel $level [list trace add variable xotcl-local-$ptr unset "::xotcl::_delete_helper [self object]"]
	}
	method hasclass {class} {
		if {![info object isa object $class]} {
			return 0
		}
		expr {[info object isa typeof [self object] $class] || [info object isa mixin [self object] $class]}
	}
	method istype {class} {
		if {![info object isa object $class]} {
			return 0
		}
		expr {[info object isa typeof [self object] $class]}
	}
	method ismixin {class} {
		if {![info object isa object $class]} {
			return 0
		}
		expr {[info object isa mixin [self object] $class]}
	}
	method info {infocmd args} {
		switch -- $infocmd {
		parent {
			set pns [namespace parent [self object]]
			if {[info object isa object $pns]} {
				return $pns
			}
			return
		}
		methods {
			set methods [info object methods [self object] -all]
			if {[llength $args]>0} {
				set mret [list]
				foreach m $methods {
					if {[string match [lindex $args 0] $m]} {
						lappend mret $m
					}
				}
				if {[llength $mret]==0} {
					return
				}
				return $mret
			} else {
				return $methods
			}
		}
		class {
			return [info object class [self object]]
		}
		children {
			set children [list]
			foreach n [namespace children [self object]] {
				if {[info object isa object $n]} {
					lappend children $n
				}
			}
			return $children
		}
		procs {
			return [info object methods [self object]]
		}
		body {
			# TODO check args count
			return [lindex [info object definition [self object] [lindex $args 0]] 1]
		}
		args {
			set ret [list]
			set def [info object definition [self object] [lindex $args 0]]
			foreach a [lindex $def 0] {
				lappend ret [lindex $a 0]
			}
			return $ret
		}
		default  {
			lassign $args method arg var_ref
			upvar $var_ref var
			foreach a [lindex [info object definition [self object] $method] 0] {
				if {[lindex $a 0] eq $arg} {
					set var [lindex $a 1]
					return 1
				}
			}
			return 0
		}
		mixin {
			return [info object mixins [self object]]
		}
		hasNamespace {
			return 1
		}
		info {
			return {parent methods class children procs body args default mixin hasNamespace info}
		}
		filter {
			return [info object filters [self object]]
		}
		default {
			error "unsupported obj info $infocmd"
		}
		}
	}
	method class {args} {
		if {[llength $args]==0} {
			info object class [self object]
		} else {
			oo::objdefine [self object] class [lindex $args 0]
		}
	}
	method requireNamespace {} {
	}
	method autoname name {
		::xotcl::_autoname $name
	}
	method unknown {args} {
		# handle calls to subcommand
		if {[llength $args]>=2} {
			set subobj [self namespace]::[lindex $args 0]
			if {[info object isa object $subobj]} {
				oo::objdefine [self object] forward [lindex $args 0] $subobj
				return [$subobj {*}[lrange $args 1 end]]
			}
		}
		::oo::Helpers::next {*}$args
	}
    export eval
  }
  
  ::oo::class create Class {
	superclass ::oo::class ::xotcl::Object
	constructor {args} {
		set superclass [list Object]
		foreach {a p} $args {
			if {$a eq "-superclass"} {
				set superclass $p
			} elseif {$a eq "-parameter"} {
				my parameter $p
			} else {
				error "unsupported class creation method $a"
			}
		}
		oo::define [self object] superclass {*}$superclass
	}
	method parameter {_parameters} {
		my variable parameter
		set parameter $_parameters
	}
	method superclass {args} {
		if {[llength $args]==0} {
			return [my info superclass]
		}
		set superclass [lindex $args 0]
		if {[llength $superclass]==0} {
			lappend superclass Object
		}
		oo::define [self object] superclass {*}$superclass
	}
	method instproc {name params body} {
		if {$name eq "destroy"} {
			oo::define [self object] destructor $body
		} else {
			oo::define [self object] method $name $params $body
		}
	}
	method new {args} {
		# todo -childof benuzte class createWithNamespace name nsname ....
		if {[lindex $args 0] eq "-childof" && [llength $args]>=2} {
			set parent [lindex $args 1]
			if {![info object isa object $parent]} {
				error "$parent is not an object"
			}
			set instname ${parent}::[::xotcl::_autoname obj]
			set inst [my createWithNamespace $instname $instname]
		} else {
			set inst [next {*}$args]
		}
		my initXOTclObject $inst {*}$args
		return $inst
	}
	method create {name args} {
		set name [::xotcl::_getObjName $name]
		set inst [my createWithNamespace $name $name {*}$args]
		my initXOTclObject $inst {*}$args
		return $inst
	}
	method alloc {name args} {
		set inst [::oo::object create $name {*}]
		oo::objdefine $inst class [self object]
		return $inst
	}
	method initXOTclObject {inst args} {
		# tclOO is setting namespace path to ::oo::Helpers by creating object
		# I found no other posibility to override tclOO next, which is slithly different to XOTcl next
		namespace eval [info object namespace $inst] {namespace path {::xotcl ::oo::Helpers}}
		my initParams $inst
		set argskip [$inst configure {*}$args]
		#puts "call init argskip=$argskip -[lrange $args 0 $argskip-1]- args -[list $args]-"
		$inst init {*}[lrange $args 0 $argskip-1]
	}
	method initParams {inst} {
		foreach sc [info class superclasses [self object]] {
			if {[info object isa typeof $sc ::xotcl::Class]} {
				$sc initParams $inst
			}
		}
		my variable parameter
		if {[info exists parameter]} {
			foreach p $parameter {
				if {[llength $p]>1} {
					set var [lindex $p 0]
					$inst set $var [lindex $p 1]
				} else {
					set var $p
				}
				if {$var eq "variable"} {
					error "you can not use parameter with name 'variable' in XOTclLight, problem with TclOO sorry"
				}
				oo::objdefine $inst forward $var $inst set $var
			}
		}
	}
	method info {infocmd args} {
		switch -- $infocmd {
		parameter {
			my variable parameter
			if {[info exists parameter]} {
				return $parameter
			}
			return
		}
		superclass {
			return [info class superclass [self object]]
		}
		instances {
			return [info class instances [self object]]
		}
		heritage {
			return [info class superclasses [self object]]
		}
		instprocs {
			# TODO filter
			return [info class methods [self object]]
		}
		instbody {
			# TODO check args count
			return [lindex [info class definition [self object] [lindex $args 0]] 1]
		}
		instargs {
			set ret [list]
			foreach a [lindex [info class definition [self object] [lindex $args 0]] 0] {
				lappend ret [lindex $a 0]
			}
			return $ret
		}
		instdefault  {
			lassign $args method arg var_ref
			upvar $var_ref var
			foreach a [lindex [info class definition [self object] $method] 0] {
				if {[lindex $a 0] eq $arg} {
					set var [lindex $a 1]
					return 1
				}
			}
			return 0
		}
		info {
			return {parameter superclass allinstances heritage instprocs instbody instargs instdefault parent methods class children procs body args default mixin hasNamespace info}
		}
		subclass {
			set children [list]
			if {![namespace exists [self object]]} {
				return
			}
			foreach n [namespace children [self object]] {
				if {[info object isa object $n] && [info object isa class $n]} {
					lappend children $n
				}
			}
			return $children
		}
		default {
			return [next $infocmd {*}$args]
		}
		}
	}
	method allinstances {} {
		info class instances [self object]
	}
	method isobject {obj} {
		expr {[info object isa object $obj] && [info object isa typeof $obj ::xotcl::Object]}
	}
	method isclass {obj} {
		info object isa class $obj
	}
	method instmixin {args} {
		if {[llength $args]>1} {
			switch [lindex $args 0] {
			assign {
				oo::define mixin [self object] -set {*}[lrange $args 1 end]
			}
			get {
				return [info class mixin [self object]]
			}
			add {
				oo::define mixin [self object] -append {*}[lrange $args 1 end]
			}
			delete {
				set cmixins [info object mixin [self object]]
				set todelete [lindex $args 1]
				set i [lsearch $cmixins $todelete]
				if {$i>=0} {
					set cmixins [lreplace $cmixins $i $i]
					oo::define mixin -set {*}$cmixins
				}
			}
			default {
				oo::define mixin [self object] {*}$args
			}
			}
		} elseif {[llength $args]==0} {
			return [info object mixin [self object]]
		} else {
			oo::define mixin [self object] {*}$args
		}
	}
	self method unknown {args} {
		uplevel Class create $args
	}
	
  }
  oo::objdefine Object class Class
  oo::objdefine Class class Class
  
  # faked @ docu object, just ignore    
  proc @ args {}
  
  namespace export Class Object @
}