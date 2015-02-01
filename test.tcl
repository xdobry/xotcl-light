# compatibility test of XOTcl and XOTclLight

package require tcltest

# manipulate the comment to test with original XOTcl and XOTclLight
#package require XOTcl
source xotcllight.tcl

namespace import ::xotcl::*

tcltest::test {xotcl-1.0} {class creation} -body {
	Class create A
	A instproc foo {} {
	    puts foo
	}
	set a [A new]
	$a foo
	$a destroy
}

tcltest::test {xotcl-1.01} {class creation unknown shortcut} -body {
	Class A1
	A1 instproc foo {} {
	    puts foo
	}
	set a [A1 new]
	$a foo
	$a destroy
}

tcltest::test {xotcl-1.01} {class procs} -body {
	Class A2
	A2 proc foo {} {
	    puts foo
	}
	A2 foo
}

tcltest::test {xotcl-1.1} {object creation} -body {
	Object create myobj
	myobj proc foo {p} {
	    my instvar a
		set a $p
		return $p
	}
	set a [myobj foo a]
	myobj destroy
	set a
} -result a

tcltest::test {xotcl-1.2} {class method calls} -body {
    Class create T1
	T1 instproc foo {} {
		my bar
		[self] bar
	}
	T1 instproc bar {} {
		return bar
	}
	set t [T1 new]
	set res [$t foo]
	$t destroy
	return $res
} -result bar

tcltest::test {xotcl-1.3} {class init instance vars} -body {
    Class create T2
	T2 instproc init {val} {
		my instvar myvar
		set myvar $val
	}
	T2 instproc foo {} {
		my instvar myvar
		return $myvar
	}
	set t [T2 new test]
	set res [$t foo]
	$t destroy
	set res
} -result test

tcltest::test {xotcl-1.4} {class superclasses} -body {
    Class create T3
	T3 instproc foo {} {
		return t3foo
	}
	Class create T4 -superclass T3
	T4 instproc foo {} {
		puts "ns orig [namespace origin next]"
		puts "ns current [namespace current]"
		next
	}
	set t [T4 new]
	set res [$t foo]
	$t destroy
	T3 destroy
	set res
} -result t3foo

tcltest::test {xotcl-1.41} {empty next} -body {
    Class create T3A
	T3A instproc foo {} {
		next
		return t3foo
	}
	set t [T3A new]
	set res [$t foo]
	$t destroy
	set res
} -result t3foo

tcltest::test {xotcl-1.42} {next without arguments} -body {
    Class create T3B
	T3B instproc foo {_p _c} {
		my instvar p
		puts "super foo $_p"
		set p $_p
		return t3foo
	}
    Class create T4B -superclass T3B
	T4B instproc foo {p c} {
		puts "invoking foo $p"
		next
	}
	set t [T4B new]
	set res [$t foo param param2]
	if {[$t set p] ne "param"} {
		error "wrong empty next arguments pass"
	}
	$t destroy
	set res
} -result t3foo


tcltest::test {xotcl-1.5} {class parameters} -body {
    Class create T5 -parameter {par1 par2}
	T5 instproc foo {} {
		my instvar par1 par2
		return $par1
	}
	set t [T5 new -par1 test1 -par2 test2]
	set res [$t foo]
	$t par2 [$t par1]
	$t destroy
	set res
} -result test1

tcltest::test {xotcl-1.51} {class parameters and init} -body {
    Class create T5A -parameter {par1 par2}
	T5A instproc init {par} {
		my instvar par2
		set par2 $par
	}
	T5A instproc foo {} {
		my instvar par2
		return $par2
	}
	set t [T5A new mypar -par1 test1 -par2 test2]
	set res [$t foo]
	$t destroy
	set res
} -result mypar

tcltest::test {xotcl-1.511} {class hierarchy parameters and init} -body {
    Class create T5F -parameter {par1 par2}
	Class create T5G -parameter {par3} -superclass T5F
	T5F instproc init {pwin} {
		my instvar win
		set win $pwin
	}
	T5G instproc init {pwin} {
		append pwin a
		next
	}
	T5G instproc foo {} {
		my instvar win
		return $win
	}
	
	set t [T5G new win -par1 test1 -par2 test2]
	set res [$t foo]
	$t destroy
	set res
} -result win

tcltest::test {xotcl-1.512} {class hierarchy parameters and init} -body {
    Class create T5H -parameter {par1 par2}
	Class create T5I -parameter {par3} -superclass T5H
	T5H instproc init {pwin} {
		my instvar win
		my specificInit $pwin
	}
	T5H instproc specificInit {pwin} {
		return $pwin
	}
	T5I instproc init {pwin} {
		puts "calling T5I init"
		next
		my finishEnd
	}
	T5I instproc specificInit {pwin} {
		puts "T5I specificInit"
		next
	}
	T5I instproc finishEnd {} {
		puts "T5I finishEnd"
	}
	
	
	set t [T5I create t5g win -par1 test1 -par2 test2]
	$t destroy
}

tcltest::test {xotcl-1.513} {class hierarchy parameters and init} -body {
    Class create T5J -parameter {par1 par2}
	Class create T5K -parameter {par3} -superclass T5J
	Class create T5L -parameter {par3} -superclass T5K
	T5J instproc init {pwin} {
		my instvar win
		set win $pwin
	}
	T5K instproc init {args} {
		next
	}
	T5L instproc init {args} {
		next
	}
	
	set t [T5L create t5g win -par1 test1 -par2 test2]
	$t destroy
}

tcltest::test {xotcl-1.52} {internal parameters usage my param} -body {
    Class create T5B -parameter par1
	T5B instproc init {} {
		my par1 set
	}
	T5B instproc foo {} {
		my par1
	}
	set t [T5B new -par1 test1]
	set res [$t foo]
	$t destroy
	set res
} -result set

tcltest::test {xotcl-1.53} {parameter heritage} -body {
    Class T5C -parameter {par1 par3}
    Class create T5D -parameter par2 -superclass T5C
	set t [T5D new -par1 test1 -par2 test2 -par3 test3]
	set res [$t par1]
	$t destroy
	set res
} -result test1

tcltest::test {xotcl-1.54} {parameter defaults} -body {
    Class T5E -parameter {{par1 1} {par2 {}}}
	T5E instproc init {} {
		my instvar par2 par1
		puts "par=$par1 par2=$par2"
		puts "self [self] self object [self object]"
	}
	T5E instproc foo {} {
		my instvar par1 par2
		list $par1 par2
	}
	set t [T5E new]
	set res [$t par1]
	if {[$t par2] ne ""} { error "export par2 is empty"}
	$t foo
	set tn [T5E create ${t}::n]
	set res [$tn par1]
	if {[$tn par2] ne ""} { error "export par2 is empty"}
	$tn foo
	set t2 [T5E create mt]
	set res [$t2 par1]
	if {[$t2 par2] ne ""} { error "export par2 is empty"}
	$t2 foo
	set tn2 [T5E new -childof $t]
	set res [$tn2 par1]
	if {[$tn2 par2] ne ""} { error "export par2 is empty"}
	$tn2 foo
	$t2 destroy
	$tn destroy
	$t destroy
	set res
} -result 1


tcltest::test {xotcl-1.6} {object variable methods shortcuts} -body {
    Class create T6
	T6 instproc foo {} {
		my instvar par1
		return $par1
	}
	set t [T6 new]
	$t set par1 test
	if {![$t exists par1]} {
		error "variable par1 should exists"
	}
	set res [$t foo]
	$t set par1
	$t set i 0
	$t incr i
	$t incr i 2
	if {[$t set i]!=3} {
		error "t should be 3"
	}
	$t destroy
	set res
} -result test

tcltest::test {xotcl-1.7} {introspection} -body {
    Class create T7
	T7 instproc foo {} {
		my instvar par1
		return $par1
	}
	set t [T6 new]
	if {![Object isobject $t]} { error "should be object" }
}

tcltest::test {xotcl-1.8} {childof creation} -body {
    Class create T8A
	Class create T8B
	set t [T8A new]
	puts "t $t"
	set tc1 [T8B new -childof $t]
	puts "tc1 -childof $tc1 t=$t parent [$tc1 info parent]"
	set tc3 [T8B create ${t}::t2]
	if {[$tc1 info parent] ne $t} { error "1 expect $t es parent"} 
	if {[$tc3 info parent] ne $t} { error "2 expect $t es parent"} 
	set children [$t info children]
	if {[llength $children]!=2} { error "expect 2 children is [list $children]"}
	if {$tc1 ni $children} { error "exprected child not found"}
	if {$tc3 ni $children} { error "exprected child not found"}
	$t destroy
	if {[Object isobject $tc1]} { error "child should be destoyed"}
	if {[Object isobject $tc3]} { error "child should be destoyed"}
}

tcltest::test {xotcl-1.9} {class api} -body {
	Class T9
	set t [T9 new]
	if {[llength [T9 allinstances]]!=1} {error "expect 1 instance"}
	T9 instproc foo {a b} {
		return $a
	}
	T9 superclass
	T9 parameter {p {p2 2}}
	if {[T9 info parameter] ne {p {p2 2}}} {error "expect 2 parameter"}
	if {[T9 superclass] ne "::xotcl::Object"} {
		error "::xotcl::Object as superclass"
	}
	Class T9A
	T9 superclass T9A
	if {[T9 superclass] ne "::T9A"} {
		error "::T9A as superclass"
	}
	set t2 [T9 alloc t9inst]
	$t2 destroy
	T9 info instances
	T9 info heritage
	if {[T9 info superclass] ne [T9 superclass]} { error "supeclass not match"}
	if {"foo" ni [T9 info instprocs]} { error "foo not in instprocs"}
	if {[T9 info instprocs foo] eq ""} { error "foo is not in filter"}
	if {[T9 info instprocs unfoo] ne ""} { error "filter does not work '[T9 info instprocs unfoo]'"}
	T9 info instbody foo
	T9 info instdefault foo a var
	if {{a b} ne [T9 info instargs foo]} { error "foo arguments not match"}
	if {![Object isclass T9]} { error "T9 should be class"}
	T9 instmixin
	T9 info subclass
}

tcltest::test {xotcl-1.10} {object api} -body {
	Object obj
	set o1 [Object autoname a]
	set o2 [Object autoname a]
	if {$o1 eq $o2} { error "names are unique"}
	obj eval {set a 2}
	#if {[obj set a] ne "2"} { error "a not set"}
	Class T10
	set t [T10 new]
	obj proc foo {a b} {
		return $a
	}
	obj info info
	obj info children
	obj info methods
	if {"foo" ni [obj info procs]} {
		error "foo not procs"
	}
	if {[obj info args foo] eq ""} {
		error "args foo not found"
	}
	if {[obj info body foo] eq ""} {
		error "body foo not found"
	}
	obj info hasNamespace
	obj info default foo a v
	obj info parent
	obj info filter
	obj info mixin
	obj mixin
	obj destroy
}

tcltest::test {xotcl-1.11} {object new -volatile} -body {
	Class T11
	Class T11A
	T11A instproc foo {} {
		return 1
	}
	T11 instproc test {} {
		set i [T11A new -volatile]
        $i foo
		return $i
	}
	set t [T11 new]
	set o [$t test]
	if {[Object isobject $o]} {
		error "-volatile does not work"
	}
	$t destroy
}

tcltest::test {xotcl-1.12} {nested objects forward invocation} -body {
	Class T12
	Class T12A
	T12A instproc foo {} {
		return 1
	}
	T12 instproc test {} {
		T12A create [self]::nested
		my nested foo
	}
	T12 instproc test2 {} {
		my nested foo
	}
	set t [T12 new]
	$t test
	$t test2
	$t destroy
}

tcltest::test {xotcl-1.121} {trace set} -body {
	set ::utest 0
	proc unsettrig {args} {
		set ::utest 1
	}
	proc test1 {} {
		set a 2
		puts "adding trace"
		trace add variable a unset unsettrig
	}
	test1
	if {$::utest != 1} {
		error "trace variable does not work"
	}

}

tcltest::test {xotcl-1.13} {object as command result full name} -body {
    Class T13
    set t [T13 new]
    puts [$t]
    $t destroy
}


tcltest::cleanupTests


