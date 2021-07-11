(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.bU.bo === region.b2.bo)
	{
		return 'on line ' + region.bU.bo;
	}
	return 'on lines ' + region.bU.bo + ' through ' + region.b2.bo;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.a2,
		impl.dh,
		impl.bF,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		az: func(record.az),
		bV: record.bV,
		bR: record.bR
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.az;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.bV;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.bR) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.a2,
		impl.dh,
		impl.bF,
		function(sendToApp, initialModel) {
			var view = impl.dk;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.a2,
		impl.dh,
		impl.bF,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.bS && impl.bS(sendToApp)
			var view = impl.dk;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.bi);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.db) && (_VirtualDom_doc.title = title = doc.db);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.ce;
	var onUrlRequest = impl.cf;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		bS: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.cl === next.cl
							&& curr.b6 === next.b6
							&& curr.ci.a === next.ci.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		a2: function(flags)
		{
			return A3(impl.a2, flags, _Browser_getUrl(), key);
		},
		dk: impl.dk,
		dh: impl.dh,
		bF: impl.bF
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { cT: 'hidden', cB: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { cT: 'mozHidden', cB: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { cT: 'msHidden', cB: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { cT: 'webkitHidden', cB: 'webkitvisibilitychange' }
		: { cT: 'hidden', cB: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		cp: _Browser_getScene(),
		cu: {
			cv: _Browser_window.pageXOffset,
			bg: _Browser_window.pageYOffset,
			dl: _Browser_doc.documentElement.clientWidth,
			cS: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		dl: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		cS: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			cp: {
				dl: node.scrollWidth,
				cS: node.scrollHeight
			},
			cu: {
				cv: node.scrollLeft,
				bg: node.scrollTop,
				dl: node.clientWidth,
				cS: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			cp: _Browser_getScene(),
			cu: {
				cv: x,
				bg: y,
				dl: _Browser_doc.documentElement.clientWidth,
				cS: _Browser_doc.documentElement.clientHeight
			},
			cF: {
				cv: x + rect.left,
				bg: y + rect.top,
				dl: rect.width,
				cS: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $author$project$Main$Tick = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$GT = 2;
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$InitTime = function (a) {
	return {$: 1, a: a};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$UserMsg = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.s) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.v),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.v);
		} else {
			var treeLen = builder.s * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.y) : builder.y;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.s);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.v) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.v);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{y: nodeList, s: (len / $elm$core$Array$branchFactor) | 0, v: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $MacCASOutreach$graphicsvg$GraphicSVG$Graphics = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {b4: fragment, b6: host, cg: path, ci: port_, cl: protocol, cm: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$document = _Browser_document;
var $elm$core$Basics$not = _Basics_not;
var $MacCASOutreach$graphicsvg$GraphicSVG$convertCoords = F2(
	function (_v0, gModel) {
		var x = _v0.a;
		var y = _v0.b;
		var sw = gModel.bG;
		var sh = gModel.bD;
		var cw = gModel.aU;
		var ch = gModel.aT;
		var aspectout = (!(!sh)) ? (sw / sh) : (4 / 3);
		var aspectin = (!(!ch)) ? (cw / ch) : (4 / 3);
		var scaledInX = _Utils_cmp(aspectout, aspectin) < 0;
		var scaledInY = _Utils_cmp(aspectout, aspectin) > 0;
		var cscale = scaledInX ? (sw / cw) : (scaledInY ? (sh / ch) : 1);
		return _Utils_Tuple2((x - (sw / 2)) / cscale, (y + (sh / 2)) / cscale);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$NoOp = {$: 3};
var $MacCASOutreach$graphicsvg$GraphicSVG$WindowResize = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Task$onError = _Scheduler_onError;
var $elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return $elm$core$Task$command(
			A2(
				$elm$core$Task$onError,
				A2(
					$elm$core$Basics$composeL,
					A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
					$elm$core$Result$Err),
				A2(
					$elm$core$Task$andThen,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
						$elm$core$Result$Ok),
					task)));
	});
var $elm$browser$Browser$Dom$getViewportOf = _Browser_getViewportOf;
var $elm$core$Basics$round = _Basics_round;
var $MacCASOutreach$graphicsvg$GraphicSVG$getViewportSize = A2(
	$elm$core$Task$attempt,
	function (rvp) {
		if (!rvp.$) {
			var vp = rvp.a;
			return $MacCASOutreach$graphicsvg$GraphicSVG$WindowResize(
				$elm$core$Maybe$Just(
					_Utils_Tuple2(
						$elm$core$Basics$round(vp.cu.dl),
						$elm$core$Basics$round(vp.cu.cS))));
		} else {
			return $MacCASOutreach$graphicsvg$GraphicSVG$NoOp;
		}
	},
	$elm$browser$Browser$Dom$getViewportOf('render'));
var $elm$core$Platform$Cmd$map = _Platform_map;
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $MacCASOutreach$graphicsvg$GraphicSVG$hiddenAppUpdate = F4(
	function (userView, userUpdate, msg, _v0) {
		var userModel = _v0.a;
		var gModel = _v0.b;
		var mapUserCmd = function (cmd) {
			return A2($elm$core$Platform$Cmd$map, $MacCASOutreach$graphicsvg$GraphicSVG$Graphics, cmd);
		};
		var _v1 = userView(userModel).bi;
		var cw = _v1.a;
		var ch = _v1.b;
		switch (msg.$) {
			case 0:
				var message = msg.a;
				var _v3 = A2(userUpdate, message, userModel);
				var newModel = _v3.a;
				var userCmds = _v3.b;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						newModel,
						_Utils_update(
							gModel,
							{aT: ch, aU: cw})),
					mapUserCmd(userCmds));
			case 1:
				var mWH = msg.a;
				if (!mWH.$) {
					var _v5 = mWH.a;
					var w = _v5.a;
					var h = _v5.b;
					return _Utils_Tuple2(
						_Utils_Tuple2(
							userModel,
							_Utils_update(
								gModel,
								{bD: h, bG: w})),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						_Utils_Tuple2(userModel, gModel),
						$MacCASOutreach$graphicsvg$GraphicSVG$getViewportSize);
				}
			case 2:
				var message = msg.a;
				var _v6 = msg.b;
				var x = _v6.a;
				var y = _v6.b;
				var _v7 = A2(
					userUpdate,
					message(
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$convertCoords,
							_Utils_Tuple2(x, y),
							gModel)),
					userModel);
				var newModel = _v7.a;
				var userCmds = _v7.b;
				return _Utils_Tuple2(
					_Utils_Tuple2(newModel, gModel),
					mapUserCmd(userCmds));
			default:
				return _Utils_Tuple2(
					_Utils_Tuple2(userModel, gModel),
					$elm$core$Platform$Cmd$none);
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$ReturnPosition = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$clipPath = $elm$svg$Svg$trustedNode('clipPath');
var $elm$svg$Svg$defs = $elm$svg$Svg$trustedNode('defs');
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$svg$Svg$Attributes$id = _VirtualDom_attribute('id');
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$svg$Svg$rect = $elm$svg$Svg$trustedNode('rect');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $MacCASOutreach$graphicsvg$GraphicSVG$cPath = F2(
	function (w, h) {
		return A2(
			$elm$svg$Svg$defs,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$clipPath,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$id('cPath')
						]),
					_List_fromArray(
						[
							A2(
							$elm$svg$Svg$rect,
							_List_fromArray(
								[
									$elm$svg$Svg$Attributes$width(
									$elm$core$String$fromFloat(w)),
									$elm$svg$Svg$Attributes$height(
									$elm$core$String$fromFloat(h)),
									$elm$svg$Svg$Attributes$x(
									$elm$core$String$fromFloat((-w) / 2)),
									$elm$svg$Svg$Attributes$y(
									$elm$core$String$fromFloat((-h) / 2))
								]),
							_List_Nil)
						]))
				]));
	});
var $elm$svg$Svg$Attributes$clipPath = _VirtualDom_attribute('clip-path');
var $MacCASOutreach$graphicsvg$GraphicSVG$Everything = {$: 11};
var $MacCASOutreach$graphicsvg$GraphicSVG$Group = function (a) {
	return {$: 7, a: a};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$Notathing = {$: 12};
var $elm$svg$Svg$a = $elm$svg$Svg$trustedNode('a');
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$RGBA = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$black = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 0, 0, 0, 1);
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$contenteditable = $elm$html$Html$Attributes$boolProperty('contentEditable');
var $MacCASOutreach$graphicsvg$GraphicSVG$pairToString = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return $elm$core$String$fromFloat(x) + (',' + $elm$core$String$fromFloat(y));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$bezierStringHelper = function (_v0) {
	var _v1 = _v0.a;
	var a = _v1.a;
	var b = _v1.b;
	var _v2 = _v0.b;
	var c = _v2.a;
	var d = _v2.b;
	return ' Q ' + ($MacCASOutreach$graphicsvg$GraphicSVG$pairToString(
		_Utils_Tuple2(a, b)) + (' ' + $MacCASOutreach$graphicsvg$GraphicSVG$pairToString(
		_Utils_Tuple2(c, d))));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$createBezierString = F2(
	function (first, list) {
		return 'M ' + ($MacCASOutreach$graphicsvg$GraphicSVG$pairToString(first) + $elm$core$String$concat(
			A2($elm$core$List$map, $MacCASOutreach$graphicsvg$GraphicSVG$bezierStringHelper, list)));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Inked = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$filled = F2(
	function (color, stencil) {
		return A3(
			$MacCASOutreach$graphicsvg$GraphicSVG$Inked,
			$elm$core$Maybe$Just(color),
			$elm$core$Maybe$Nothing,
			stencil);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Move = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$move = F2(
	function (disp, shape) {
		return A2($MacCASOutreach$graphicsvg$GraphicSVG$Move, disp, shape);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Rect = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$rect = F2(
	function (w, h) {
		return A2($MacCASOutreach$graphicsvg$GraphicSVG$Rect, w, h);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$createGraphX = F5(
	function (h, s, th, c, x) {
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(x * s, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				c,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, th, h)));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$createGraphY = F5(
	function (w, s, th, c, y) {
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(0, y * s),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				c,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, th)));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$group = function (shapes) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$Group(shapes);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$createGraph = F4(
	function (_v0, s, th, c) {
		var w = _v0.a;
		var h = _v0.b;
		var syi = $elm$core$Basics$ceiling(h / (s * 2));
		var ylisti = A2($elm$core$List$range, -syi, syi);
		var sxi = $elm$core$Basics$ceiling(w / (s * 2));
		var xlisti = A2($elm$core$List$range, -sxi, sxi);
		return $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_Utils_ap(
				A2(
					$elm$core$List$map,
					A2(
						$elm$core$Basics$composeL,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$createGraphX, h, s, th, c),
						$elm$core$Basics$toFloat),
					xlisti),
				A2(
					$elm$core$List$map,
					A2(
						$elm$core$Basics$composeL,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$createGraphY, w, s, th, c),
						$elm$core$Basics$toFloat),
					ylisti)));
	});
var $elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var $elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$ellipse = $elm$svg$Svg$trustedNode('ellipse');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$Attributes$fillOpacity = _VirtualDom_attribute('fill-opacity');
var $elm$svg$Svg$Attributes$fontSize = _VirtualDom_attribute('font-size');
var $elm$svg$Svg$foreignObject = $elm$svg$Svg$trustedNode('foreignObject');
var $elm$svg$Svg$g = $elm$svg$Svg$trustedNode('g');
var $MacCASOutreach$graphicsvg$GraphicSVG$ident = _Utils_Tuple2(
	_Utils_Tuple3(1, 0, 0),
	_Utils_Tuple3(0, 1, 0));
var $elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						$elm$core$List$cons,
						sep,
						A2($elm$core$List$cons, x, rest));
				});
			var spersed = A3($elm$core$List$foldr, step, _List_Nil, tl);
			return A2($elm$core$List$cons, hd, spersed);
		}
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$svg$Svg$mask = $elm$svg$Svg$trustedNode('mask');
var $elm$svg$Svg$Attributes$mask = _VirtualDom_attribute('mask');
var $MacCASOutreach$graphicsvg$GraphicSVG$matrixMult = F2(
	function (_v0, _v3) {
		var _v1 = _v0.a;
		var a = _v1.a;
		var c = _v1.b;
		var e = _v1.c;
		var _v2 = _v0.b;
		var b = _v2.a;
		var d = _v2.b;
		var f = _v2.c;
		var _v4 = _v3.a;
		var a1 = _v4.a;
		var c1 = _v4.b;
		var e1 = _v4.c;
		var _v5 = _v3.b;
		var b1 = _v5.a;
		var d1 = _v5.b;
		var f1 = _v5.c;
		return _Utils_Tuple2(
			_Utils_Tuple3((a * a1) + (c * b1), (a * c1) + (c * d1), (e + (a * e1)) + (c * f1)),
			_Utils_Tuple3((b * a1) + (d * b1), (b * c1) + (d * d1), (f + (b * e1)) + (d * f1)));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$mkAlpha = function (_v0) {
	var a = _v0.d;
	return $elm$core$String$fromFloat(a);
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $MacCASOutreach$graphicsvg$GraphicSVG$toHexHelper = function (dec) {
	switch (dec) {
		case 0:
			return '0';
		case 1:
			return '1';
		case 2:
			return '2';
		case 3:
			return '3';
		case 4:
			return '4';
		case 5:
			return '5';
		case 6:
			return '6';
		case 7:
			return '7';
		case 8:
			return '8';
		case 9:
			return '9';
		case 10:
			return 'A';
		case 11:
			return 'B';
		case 12:
			return 'C';
		case 13:
			return 'D';
		case 14:
			return 'E';
		case 15:
			return 'F';
		default:
			return '';
	}
};
var $MacCASOutreach$graphicsvg$GraphicSVG$toHex = function (dec) {
	var second = A2($elm$core$Basics$modBy, 16, dec);
	var first = (dec / 16) | 0;
	return _Utils_ap(
		$MacCASOutreach$graphicsvg$GraphicSVG$toHexHelper(first),
		$MacCASOutreach$graphicsvg$GraphicSVG$toHexHelper(second));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$mkRGB = function (_v0) {
	var r = _v0.a;
	var g = _v0.b;
	var b = _v0.c;
	return '#' + ($MacCASOutreach$graphicsvg$GraphicSVG$toHex(
		$elm$core$Basics$round(r)) + ($MacCASOutreach$graphicsvg$GraphicSVG$toHex(
		$elm$core$Basics$round(g)) + $MacCASOutreach$graphicsvg$GraphicSVG$toHex(
		$elm$core$Basics$round(b))));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$moveT = F2(
	function (_v0, _v1) {
		var u = _v0.a;
		var v = _v0.b;
		var _v2 = _v1.a;
		var a = _v2.a;
		var c = _v2.b;
		var tx = _v2.c;
		var _v3 = _v1.b;
		var b = _v3.a;
		var d = _v3.b;
		var ty = _v3.c;
		return _Utils_Tuple2(
			_Utils_Tuple3(a, c, (tx + (a * u)) + (c * v)),
			_Utils_Tuple3(b, d, (ty + (b * u)) + (d * v)));
	});
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $MacCASOutreach$graphicsvg$GraphicSVG$mousePosDecoder = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (x, y) {
			return _Utils_Tuple2(x, -y);
		}),
	A2($elm$json$Json$Decode$field, 'offsetX', $elm$json$Json$Decode$float),
	A2($elm$json$Json$Decode$field, 'offsetY', $elm$json$Json$Decode$float));
var $MacCASOutreach$graphicsvg$GraphicSVG$onEnterAt = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseover',
		A2($elm$json$Json$Decode$map, msg, $MacCASOutreach$graphicsvg$GraphicSVG$mousePosDecoder));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onLeaveAt = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseleave',
		A2($elm$json$Json$Decode$map, msg, $MacCASOutreach$graphicsvg$GraphicSVG$mousePosDecoder));
};
var $elm$html$Html$Events$onMouseDown = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mousedown',
		$elm$json$Json$Decode$succeed(msg));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onMouseDownAt = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mousedown',
		A2($elm$json$Json$Decode$map, msg, $MacCASOutreach$graphicsvg$GraphicSVG$mousePosDecoder));
};
var $elm$html$Html$Events$onMouseEnter = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseenter',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$onMouseLeave = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseleave',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$onMouseUp = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseup',
		$elm$json$Json$Decode$succeed(msg));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onMouseUpAt = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseup',
		A2($elm$json$Json$Decode$map, msg, $MacCASOutreach$graphicsvg$GraphicSVG$mousePosDecoder));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onMoveAt = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mousemove',
		A2($elm$json$Json$Decode$map, msg, $MacCASOutreach$graphicsvg$GraphicSVG$mousePosDecoder));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onTapAt = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		A2($elm$json$Json$Decode$map, msg, $MacCASOutreach$graphicsvg$GraphicSVG$mousePosDecoder));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onTouchEnd = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'touchend',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 2, a: a};
};
var $elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$TouchPos = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $MacCASOutreach$graphicsvg$GraphicSVG$touchDecoder = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$json$Json$Decode$at,
			_List_fromArray(
				['touches', '0']),
			A3(
				$elm$json$Json$Decode$map2,
				$MacCASOutreach$graphicsvg$GraphicSVG$TouchPos,
				A2($elm$json$Json$Decode$field, 'pageX', $elm$json$Json$Decode$float),
				A2($elm$json$Json$Decode$field, 'pageY', $elm$json$Json$Decode$float))),
			A3(
			$elm$json$Json$Decode$map2,
			$MacCASOutreach$graphicsvg$GraphicSVG$TouchPos,
			A2($elm$json$Json$Decode$field, 'pageX', $elm$json$Json$Decode$float),
			A2($elm$json$Json$Decode$field, 'pageY', $elm$json$Json$Decode$float))
		]));
var $MacCASOutreach$graphicsvg$GraphicSVG$touchToPair = function (tp) {
	var x = tp.a;
	var y = tp.b;
	return _Utils_Tuple2(x, -y);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onTouchMove = function (msg) {
	return A2(
		$elm$html$Html$Events$preventDefaultOn,
		'touchmove',
		A2(
			$elm$json$Json$Decode$map,
			function (a) {
				return _Utils_Tuple2(
					A2($elm$core$Basics$composeL, msg, $MacCASOutreach$graphicsvg$GraphicSVG$touchToPair)(a),
					true);
			},
			$MacCASOutreach$graphicsvg$GraphicSVG$touchDecoder));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onTouchStart = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'touchstart',
		$elm$json$Json$Decode$succeed(msg));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$onTouchStartAt = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'touchstart',
		A2(
			$elm$json$Json$Decode$map,
			A2($elm$core$Basics$composeL, msg, $MacCASOutreach$graphicsvg$GraphicSVG$touchToPair),
			$MacCASOutreach$graphicsvg$GraphicSVG$touchDecoder));
};
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$svg$Svg$Attributes$points = _VirtualDom_attribute('points');
var $elm$svg$Svg$polygon = $elm$svg$Svg$trustedNode('polygon');
var $elm$svg$Svg$polyline = $elm$svg$Svg$trustedNode('polyline');
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var $MacCASOutreach$graphicsvg$GraphicSVG$AlphaMask = F2(
	function (a, b) {
		return {$: 9, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Clip = F2(
	function (a, b) {
		return {$: 10, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$EnterAt = F2(
	function (a, b) {
		return {$: 17, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$EnterShape = F2(
	function (a, b) {
		return {$: 16, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Exit = F2(
	function (a, b) {
		return {$: 18, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$ExitAt = F2(
	function (a, b) {
		return {$: 19, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$ForeignObject = F3(
	function (a, b, c) {
		return {$: 1, a: a, b: b, c: c};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$GraphPaper = F3(
	function (a, b, c) {
		return {$: 30, a: a, b: b, c: c};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$GroupOutline = function (a) {
	return {$: 8, a: a};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$Link = F2(
	function (a, b) {
		return {$: 13, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$MouseDown = F2(
	function (a, b) {
		return {$: 20, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$MouseDownAt = F2(
	function (a, b) {
		return {$: 21, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$MouseUp = F2(
	function (a, b) {
		return {$: 22, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$MouseUpAt = F2(
	function (a, b) {
		return {$: 23, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$MoveOverAt = F2(
	function (a, b) {
		return {$: 24, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Rotate = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Scale = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Skew = F3(
	function (a, b, c) {
		return {$: 5, a: a, b: b, c: c};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Tap = F2(
	function (a, b) {
		return {$: 14, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$TapAt = F2(
	function (a, b) {
		return {$: 15, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$TouchEnd = F2(
	function (a, b) {
		return {$: 26, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$TouchEndAt = F2(
	function (a, b) {
		return {$: 28, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$TouchMoveAt = F2(
	function (a, b) {
		return {$: 29, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$TouchStart = F2(
	function (a, b) {
		return {$: 25, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$TouchStartAt = F2(
	function (a, b) {
		return {$: 27, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Transformed = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$repaint = F2(
	function (color, shape) {
		switch (shape.$) {
			case 0:
				if (shape.b.$ === 1) {
					var _v1 = shape.b;
					var st = shape.c;
					return A3(
						$MacCASOutreach$graphicsvg$GraphicSVG$Inked,
						$elm$core$Maybe$Just(color),
						$elm$core$Maybe$Nothing,
						st);
				} else {
					var _v2 = shape.b.a;
					var lt = _v2.a;
					var st = shape.c;
					return A3(
						$MacCASOutreach$graphicsvg$GraphicSVG$Inked,
						$elm$core$Maybe$Just(color),
						$elm$core$Maybe$Just(
							_Utils_Tuple2(lt, color)),
						st);
				}
			case 2:
				var s = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Move,
					s,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 3:
				var r = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Rotate,
					r,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 4:
				var sx = shape.a;
				var sy = shape.b;
				var sh = shape.c;
				return A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$Scale,
					sx,
					sy,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 5:
				var skx = shape.a;
				var sky = shape.b;
				var sh = shape.c;
				return A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$Skew,
					skx,
					sky,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 6:
				var tm = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Transformed,
					tm,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 7:
				var shapes = shape.a;
				return $MacCASOutreach$graphicsvg$GraphicSVG$Group(
					A2(
						$elm$core$List$map,
						$MacCASOutreach$graphicsvg$GraphicSVG$repaint(color),
						shapes));
			case 8:
				var cmbndshp = shape.a;
				return $MacCASOutreach$graphicsvg$GraphicSVG$GroupOutline(
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, cmbndshp));
			case 13:
				var s = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Link,
					s,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 9:
				var reg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$AlphaMask,
					reg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 10:
				var reg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Clip,
					reg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 14:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Tap,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 15:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TapAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 16:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$EnterShape,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 17:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$EnterAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 18:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Exit,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 19:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$ExitAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 20:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$MouseDown,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 21:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$MouseDownAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 22:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$MouseUp,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 23:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$MouseUpAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 24:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$MoveOverAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 25:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TouchStart,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 26:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TouchEnd,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 27:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TouchStartAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 28:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TouchEndAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 29:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TouchMoveAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, color, sh));
			case 1:
				var w = shape.a;
				var h = shape.b;
				var htm = shape.c;
				return A3($MacCASOutreach$graphicsvg$GraphicSVG$ForeignObject, w, h, htm);
			case 11:
				return $MacCASOutreach$graphicsvg$GraphicSVG$Everything;
			case 12:
				return $MacCASOutreach$graphicsvg$GraphicSVG$Notathing;
			default:
				var s = shape.a;
				var th = shape.b;
				return A3($MacCASOutreach$graphicsvg$GraphicSVG$GraphPaper, s, th, color);
		}
	});
var $elm$core$Basics$cos = _Basics_cos;
var $elm$core$Basics$sin = _Basics_sin;
var $MacCASOutreach$graphicsvg$GraphicSVG$rotateT = F2(
	function (rad, _v0) {
		var _v1 = _v0.a;
		var a = _v1.a;
		var c = _v1.b;
		var tx = _v1.c;
		var _v2 = _v0.b;
		var b = _v2.a;
		var d = _v2.b;
		var ty = _v2.c;
		var sinX = $elm$core$Basics$sin(rad);
		var cosX = $elm$core$Basics$cos(rad);
		return _Utils_Tuple2(
			_Utils_Tuple3((a * cosX) + (c * sinX), (c * cosX) - (a * sinX), tx),
			_Utils_Tuple3((b * cosX) + (d * sinX), (d * cosX) - (b * sinX), ty));
	});
var $elm$svg$Svg$Attributes$rx = _VirtualDom_attribute('rx');
var $elm$svg$Svg$Attributes$ry = _VirtualDom_attribute('ry');
var $MacCASOutreach$graphicsvg$GraphicSVG$scaleT = F3(
	function (sx, sy, _v0) {
		var _v1 = _v0.a;
		var a = _v1.a;
		var c = _v1.b;
		var tx = _v1.c;
		var _v2 = _v0.b;
		var b = _v2.a;
		var d = _v2.b;
		var ty = _v2.c;
		return _Utils_Tuple2(
			_Utils_Tuple3(a * sx, c * sy, tx),
			_Utils_Tuple3(b * sx, d * sy, ty));
	});
var $elm$core$Basics$tan = _Basics_tan;
var $MacCASOutreach$graphicsvg$GraphicSVG$skewT = F3(
	function (skx, sky, _v0) {
		var _v1 = _v0.a;
		var a = _v1.a;
		var c = _v1.b;
		var tx = _v1.c;
		var _v2 = _v0.b;
		var b = _v2.a;
		var d = _v2.b;
		var ty = _v2.c;
		var tanY = $elm$core$Basics$tan(-sky);
		var tanX = $elm$core$Basics$tan(-skx);
		return _Utils_Tuple2(
			_Utils_Tuple3(a + (c * tanY), c + (a * tanX), tx),
			_Utils_Tuple3(b + (d * tanY), d + (b * tanX), ty));
	});
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeDasharray = _VirtualDom_attribute('stroke-dasharray');
var $elm$svg$Svg$Attributes$strokeOpacity = _VirtualDom_attribute('stroke-opacity');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $elm$svg$Svg$Attributes$style = _VirtualDom_attribute('style');
var $elm$svg$Svg$Attributes$target = _VirtualDom_attribute('target');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$svg$Svg$text = $elm$virtual_dom$VirtualDom$text;
var $elm$svg$Svg$Attributes$textAnchor = _VirtualDom_attribute('text-anchor');
var $elm$svg$Svg$text_ = $elm$svg$Svg$trustedNode('text');
var $elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var $MacCASOutreach$graphicsvg$GraphicSVG$white = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 255, 255, 255, 1);
var $elm$svg$Svg$Attributes$xlinkHref = function (value) {
	return A3(
		_VirtualDom_attributeNS,
		'http://www.w3.org/1999/xlink',
		'xlink:href',
		_VirtualDom_noJavaScriptUri(value));
};
var $elm$svg$Svg$Attributes$xmlSpace = A2(_VirtualDom_attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:space');
var $MacCASOutreach$graphicsvg$GraphicSVG$createSVG = F7(
	function (id, w, h, trans, msgWrapper, positionWrapper, shape) {
		createSVG:
		while (true) {
			switch (shape.$) {
				case 0:
					var fillClr = shape.a;
					var lt = shape.b;
					var stencil = shape.c;
					var strokeAttrs = function () {
						if (lt.$ === 1) {
							return _List_Nil;
						} else {
							switch (lt.a.a.$) {
								case 1:
									var _v11 = lt.a;
									var th = _v11.a.a;
									var strokeClr = _v11.b;
									var nonStroke = function () {
										var _v12 = strokeClr;
										var opcty = _v12.d;
										return (th <= 0) || (opcty <= 0);
									}();
									return nonStroke ? _List_Nil : _List_fromArray(
										[
											$elm$svg$Svg$Attributes$strokeWidth(
											$elm$core$String$fromFloat(th)),
											$elm$svg$Svg$Attributes$stroke(
											$MacCASOutreach$graphicsvg$GraphicSVG$mkRGB(strokeClr)),
											$elm$svg$Svg$Attributes$strokeOpacity(
											$MacCASOutreach$graphicsvg$GraphicSVG$mkAlpha(strokeClr))
										]);
								case 2:
									var _v13 = lt.a;
									var _v14 = _v13.a;
									var dashes = _v14.a;
									var th = _v14.b;
									var strokeClr = _v13.b;
									var nonStroke = function () {
										var _v15 = strokeClr;
										var opcty = _v15.d;
										return (th <= 0) || ((opcty <= 0) || A2(
											$elm$core$List$all,
											function (_v16) {
												var on = _v16.a;
												return !on;
											},
											dashes));
									}();
									return nonStroke ? _List_Nil : _Utils_ap(
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$strokeWidth(
												$elm$core$String$fromFloat(th)),
												$elm$svg$Svg$Attributes$stroke(
												$MacCASOutreach$graphicsvg$GraphicSVG$mkRGB(strokeClr)),
												$elm$svg$Svg$Attributes$strokeOpacity(
												$MacCASOutreach$graphicsvg$GraphicSVG$mkAlpha(strokeClr))
											]),
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$strokeDasharray(
												$elm$core$String$concat(
													A2(
														$elm$core$List$intersperse,
														',',
														A2($elm$core$List$map, $MacCASOutreach$graphicsvg$GraphicSVG$pairToString, dashes))))
											]));
								default:
									var _v17 = lt.a;
									var _v18 = _v17.a;
									return _List_Nil;
							}
						}
					}();
					var nonexistBody = function () {
						if (fillClr.$ === 1) {
							return true;
						} else {
							return false;
						}
					}();
					var clrAttrs = function () {
						if (fillClr.$ === 1) {
							return _List_fromArray(
								[
									$elm$svg$Svg$Attributes$fill('none')
								]);
						} else {
							var bodyClr = fillClr.a;
							return _List_fromArray(
								[
									$elm$svg$Svg$Attributes$fill(
									$MacCASOutreach$graphicsvg$GraphicSVG$mkRGB(bodyClr)),
									$elm$svg$Svg$Attributes$fillOpacity(
									$MacCASOutreach$graphicsvg$GraphicSVG$mkAlpha(bodyClr))
								]);
						}
					}();
					var _v1 = trans;
					var _v2 = _v1.a;
					var a = _v2.a;
					var c = _v2.b;
					var tx = _v2.c;
					var _v3 = _v1.b;
					var b = _v3.a;
					var d = _v3.b;
					var ty = _v3.c;
					var transAttrs = _List_fromArray(
						[
							$elm$svg$Svg$Attributes$transform(
							'matrix(' + ($elm$core$String$concat(
								A2(
									$elm$core$List$intersperse,
									',',
									A2(
										$elm$core$List$map,
										$elm$core$String$fromFloat,
										_List_fromArray(
											[a, -b, c, -d, tx, -ty])))) + ')'))
						]);
					var attrs = _Utils_ap(
						transAttrs,
						_Utils_ap(clrAttrs, strokeAttrs));
					if (nonexistBody && $elm$core$List$isEmpty(strokeAttrs)) {
						return A2($elm$svg$Svg$g, _List_Nil, _List_Nil);
					} else {
						switch (stencil.$) {
							case 0:
								var r = stencil.a;
								return A2(
									$elm$svg$Svg$circle,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$cx('0'),
												$elm$svg$Svg$Attributes$cy('0'),
												$elm$svg$Svg$Attributes$r(
												$elm$core$String$fromFloat(r))
											]),
										attrs),
									_List_Nil);
							case 1:
								var rw = stencil.a;
								var rh = stencil.b;
								return A2(
									$elm$svg$Svg$rect,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$x(
												$elm$core$String$fromFloat((-rw) / 2)),
												$elm$svg$Svg$Attributes$y(
												$elm$core$String$fromFloat((-rh) / 2)),
												$elm$svg$Svg$Attributes$width(
												$elm$core$String$fromFloat(rw)),
												$elm$svg$Svg$Attributes$height(
												$elm$core$String$fromFloat(rh))
											]),
										attrs),
									_List_Nil);
							case 2:
								var rw = stencil.a;
								var rh = stencil.b;
								var r = stencil.c;
								return A2(
									$elm$svg$Svg$rect,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$x(
												$elm$core$String$fromFloat((-rw) / 2)),
												$elm$svg$Svg$Attributes$y(
												$elm$core$String$fromFloat((-rh) / 2)),
												$elm$svg$Svg$Attributes$rx(
												$elm$core$String$fromFloat(r)),
												$elm$svg$Svg$Attributes$ry(
												$elm$core$String$fromFloat(r)),
												$elm$svg$Svg$Attributes$width(
												$elm$core$String$fromFloat(rw)),
												$elm$svg$Svg$Attributes$height(
												$elm$core$String$fromFloat(rh))
											]),
										attrs),
									_List_Nil);
							case 3:
								var ow = stencil.a;
								var oh = stencil.b;
								return A2(
									$elm$svg$Svg$ellipse,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$cx('0'),
												$elm$svg$Svg$Attributes$cy('0'),
												$elm$svg$Svg$Attributes$rx(
												$elm$core$String$fromFloat(0.5 * ow)),
												$elm$svg$Svg$Attributes$ry(
												$elm$core$String$fromFloat(0.5 * oh))
											]),
										attrs),
									_List_Nil);
							case 5:
								var vertices = stencil.a;
								return A2(
									$elm$svg$Svg$polygon,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$points(
												$elm$core$String$concat(
													A2(
														$elm$core$List$intersperse,
														' ',
														A2($elm$core$List$map, $MacCASOutreach$graphicsvg$GraphicSVG$pairToString, vertices))))
											]),
										attrs),
									_List_Nil);
							case 6:
								var vertices = stencil.a;
								return A2(
									$elm$svg$Svg$polyline,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$points(
												$elm$core$String$concat(
													A2(
														$elm$core$List$intersperse,
														' ',
														A2($elm$core$List$map, $MacCASOutreach$graphicsvg$GraphicSVG$pairToString, vertices))))
											]),
										attrs),
									_List_Nil);
							case 4:
								var start = stencil.a;
								var pts = stencil.b;
								return A2(
									$elm$svg$Svg$path,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$d(
												A2($MacCASOutreach$graphicsvg$GraphicSVG$createBezierString, start, pts))
											]),
										attrs),
									_List_Nil);
							default:
								var _v5 = stencil.a;
								var si = _v5.a;
								var bo = _v5.b;
								var i = _v5.c;
								var u = _v5.d;
								var s = _v5.e;
								var sel = _v5.f;
								var f = _v5.g;
								var align = _v5.h;
								var str = stencil.b;
								var txtDec = (u && s) ? 'text-decoration: underline line-through;' : (u ? 'text-decoration: underline;' : (s ? 'text-decoration: line-through;' : ''));
								var select = (!sel) ? '-webkit-touch-callout: none;\n-webkit-user-select: none;\n-khtml-user-select: none;\n-moz-user-select: none;\n-ms-user-select: none;\nuser-select: none;cursor: default;' : '';
								var it = i ? 'font-style: italic;' : '';
								var font = function () {
									switch (f.$) {
										case 1:
											return 'sans-serif;';
										case 0:
											return 'serif;';
										case 2:
											return 'monospace;';
										default:
											var fStr = f.a;
											return fStr + ';';
									}
								}();
								var bol = bo ? 'font-weight: bold;' : '';
								var sty = bol + (it + (txtDec + ('font-family: ' + (font + select))));
								var anchor = function () {
									switch (align) {
										case 1:
											return 'middle';
										case 0:
											return 'start';
										default:
											return 'end';
									}
								}();
								return A2(
									$elm$svg$Svg$text_,
									_Utils_ap(
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$x('0'),
												$elm$svg$Svg$Attributes$y('0'),
												$elm$svg$Svg$Attributes$style(sty),
												$elm$svg$Svg$Attributes$fontSize(
												$elm$core$String$fromFloat(si)),
												$elm$svg$Svg$Attributes$textAnchor(anchor),
												$elm$html$Html$Attributes$contenteditable(true)
											]),
										_Utils_ap(
											_List_fromArray(
												[
													$elm$svg$Svg$Attributes$transform(
													'matrix(' + ($elm$core$String$concat(
														A2(
															$elm$core$List$intersperse,
															',',
															A2(
																$elm$core$List$map,
																$elm$core$String$fromFloat,
																_List_fromArray(
																	[a, -b, -c, d, tx, -ty])))) + ')'))
												]),
											_Utils_ap(
												_List_fromArray(
													[
														$elm$svg$Svg$Attributes$xmlSpace('preserve')
													]),
												_Utils_ap(clrAttrs, strokeAttrs)))),
									_List_fromArray(
										[
											$elm$svg$Svg$text(str)
										]));
						}
					}
				case 1:
					var fw = shape.a;
					var fh = shape.b;
					var htm = shape.c;
					var _v19 = trans;
					var _v20 = _v19.a;
					var a = _v20.a;
					var c = _v20.b;
					var tx = _v20.c;
					var _v21 = _v19.b;
					var b = _v21.a;
					var d = _v21.b;
					var ty = _v21.c;
					return A2(
						$elm$svg$Svg$foreignObject,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$width(
								$elm$core$String$fromFloat(fw)),
								$elm$svg$Svg$Attributes$height(
								$elm$core$String$fromFloat(fh)),
								$elm$svg$Svg$Attributes$transform(
								'matrix(' + ($elm$core$String$concat(
									A2(
										$elm$core$List$intersperse,
										',',
										A2(
											$elm$core$List$map,
											$elm$core$String$fromFloat,
											_List_fromArray(
												[a, -b, -c, d, tx, -ty])))) + ')'))
							]),
						_List_fromArray(
							[
								A2($elm$html$Html$map, msgWrapper, htm)
							]));
				case 2:
					var v = shape.a;
					var sh = shape.b;
					var $temp$id = id,
						$temp$w = w,
						$temp$h = h,
						$temp$trans = A2($MacCASOutreach$graphicsvg$GraphicSVG$moveT, v, trans),
						$temp$msgWrapper = msgWrapper,
						$temp$positionWrapper = positionWrapper,
						$temp$shape = sh;
					id = $temp$id;
					w = $temp$w;
					h = $temp$h;
					trans = $temp$trans;
					msgWrapper = $temp$msgWrapper;
					positionWrapper = $temp$positionWrapper;
					shape = $temp$shape;
					continue createSVG;
				case 11:
					var $temp$id = id,
						$temp$w = w,
						$temp$h = h,
						$temp$trans = $MacCASOutreach$graphicsvg$GraphicSVG$ident,
						$temp$msgWrapper = msgWrapper,
						$temp$positionWrapper = positionWrapper,
						$temp$shape = A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h));
					id = $temp$id;
					w = $temp$w;
					h = $temp$h;
					trans = $temp$trans;
					msgWrapper = $temp$msgWrapper;
					positionWrapper = $temp$positionWrapper;
					shape = $temp$shape;
					continue createSVG;
				case 12:
					var $temp$id = id,
						$temp$w = w,
						$temp$h = h,
						$temp$trans = $MacCASOutreach$graphicsvg$GraphicSVG$ident,
						$temp$msgWrapper = msgWrapper,
						$temp$positionWrapper = positionWrapper,
						$temp$shape = A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$black,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h));
					id = $temp$id;
					w = $temp$w;
					h = $temp$h;
					trans = $temp$trans;
					msgWrapper = $temp$msgWrapper;
					positionWrapper = $temp$positionWrapper;
					shape = $temp$shape;
					continue createSVG;
				case 3:
					var deg = shape.a;
					var sh = shape.b;
					var $temp$id = id,
						$temp$w = w,
						$temp$h = h,
						$temp$trans = A2($MacCASOutreach$graphicsvg$GraphicSVG$rotateT, deg, trans),
						$temp$msgWrapper = msgWrapper,
						$temp$positionWrapper = positionWrapper,
						$temp$shape = sh;
					id = $temp$id;
					w = $temp$w;
					h = $temp$h;
					trans = $temp$trans;
					msgWrapper = $temp$msgWrapper;
					positionWrapper = $temp$positionWrapper;
					shape = $temp$shape;
					continue createSVG;
				case 4:
					var sx = shape.a;
					var sy = shape.b;
					var sh = shape.c;
					var $temp$id = id,
						$temp$w = w,
						$temp$h = h,
						$temp$trans = A3($MacCASOutreach$graphicsvg$GraphicSVG$scaleT, sx, sy, trans),
						$temp$msgWrapper = msgWrapper,
						$temp$positionWrapper = positionWrapper,
						$temp$shape = sh;
					id = $temp$id;
					w = $temp$w;
					h = $temp$h;
					trans = $temp$trans;
					msgWrapper = $temp$msgWrapper;
					positionWrapper = $temp$positionWrapper;
					shape = $temp$shape;
					continue createSVG;
				case 5:
					var sx = shape.a;
					var sy = shape.b;
					var sh = shape.c;
					var $temp$id = id,
						$temp$w = w,
						$temp$h = h,
						$temp$trans = A3($MacCASOutreach$graphicsvg$GraphicSVG$skewT, sx, sy, trans),
						$temp$msgWrapper = msgWrapper,
						$temp$positionWrapper = positionWrapper,
						$temp$shape = sh;
					id = $temp$id;
					w = $temp$w;
					h = $temp$h;
					trans = $temp$trans;
					msgWrapper = $temp$msgWrapper;
					positionWrapper = $temp$positionWrapper;
					shape = $temp$shape;
					continue createSVG;
				case 6:
					var tm = shape.a;
					var sh = shape.b;
					var $temp$id = id,
						$temp$w = w,
						$temp$h = h,
						$temp$trans = A2($MacCASOutreach$graphicsvg$GraphicSVG$matrixMult, trans, tm),
						$temp$msgWrapper = msgWrapper,
						$temp$positionWrapper = positionWrapper,
						$temp$shape = sh;
					id = $temp$id;
					w = $temp$w;
					h = $temp$h;
					trans = $temp$trans;
					msgWrapper = $temp$msgWrapper;
					positionWrapper = $temp$positionWrapper;
					shape = $temp$shape;
					continue createSVG;
				case 13:
					var href = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$a,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$xlinkHref(href),
								$elm$svg$Svg$Attributes$target('_blank')
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 9:
					var region = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$svg$Svg$defs,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$svg$Svg$mask,
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$id('m' + id)
											]),
										_List_fromArray(
											[
												A7(
												$MacCASOutreach$graphicsvg$GraphicSVG$createSVG,
												id + 'm',
												w,
												h,
												trans,
												msgWrapper,
												positionWrapper,
												$MacCASOutreach$graphicsvg$GraphicSVG$Group(
													_List_fromArray(
														[
															$MacCASOutreach$graphicsvg$GraphicSVG$Everything,
															A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, $MacCASOutreach$graphicsvg$GraphicSVG$black, region)
														])))
											]))
									])),
								A2(
								$elm$svg$Svg$g,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$mask('url(#m' + (id + ')'))
									]),
								_List_fromArray(
									[
										A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id + 'mm', w, h, trans, msgWrapper, positionWrapper, sh)
									]))
							]));
				case 10:
					var region = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$svg$Svg$defs,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$svg$Svg$mask,
										_List_fromArray(
											[
												$elm$svg$Svg$Attributes$id('c' + id)
											]),
										_List_fromArray(
											[
												A7(
												$MacCASOutreach$graphicsvg$GraphicSVG$createSVG,
												id + 'c',
												w,
												h,
												trans,
												msgWrapper,
												positionWrapper,
												$MacCASOutreach$graphicsvg$GraphicSVG$Group(
													_List_fromArray(
														[
															$MacCASOutreach$graphicsvg$GraphicSVG$Notathing,
															A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, $MacCASOutreach$graphicsvg$GraphicSVG$white, region)
														])))
											]))
									])),
								A2(
								$elm$svg$Svg$g,
								_List_fromArray(
									[
										$elm$svg$Svg$Attributes$mask('url(#c' + (id + ')'))
									]),
								_List_fromArray(
									[
										A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id + 'cc', w, h, trans, msgWrapper, positionWrapper, sh)
									]))
							]));
				case 14:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								msgWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 15:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onTapAt(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 16:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$elm$html$Html$Events$onMouseEnter(
								msgWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 17:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onEnterAt(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 18:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$elm$html$Html$Events$onMouseLeave(
								msgWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 19:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onLeaveAt(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 20:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$elm$html$Html$Events$onMouseDown(
								msgWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 21:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onMouseDownAt(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 22:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$elm$html$Html$Events$onMouseUp(
								msgWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 23:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onMouseUpAt(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 24:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onMoveAt(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 25:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onTouchStart(
								msgWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 26:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onTouchEnd(
								msgWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 27:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onTouchStartAt(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 28:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onTouchStartAt(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 29:
					var msg = shape.a;
					var sh = shape.b;
					return A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$MacCASOutreach$graphicsvg$GraphicSVG$onTouchMove(
								positionWrapper(msg))
							]),
						_List_fromArray(
							[
								A7($MacCASOutreach$graphicsvg$GraphicSVG$createSVG, id, w, h, trans, msgWrapper, positionWrapper, sh)
							]));
				case 7:
					var shapes = shape.a;
					return A2(
						$elm$svg$Svg$g,
						_List_Nil,
						A2(
							$elm$core$List$indexedMap,
							function (n) {
								return A6(
									$MacCASOutreach$graphicsvg$GraphicSVG$createSVG,
									id + ('g' + $elm$core$String$fromInt(n)),
									w,
									h,
									trans,
									msgWrapper,
									positionWrapper);
							},
							shapes));
				case 8:
					var cmbndshp = shape.a;
					var $temp$id = id,
						$temp$w = w,
						$temp$h = h,
						$temp$trans = trans,
						$temp$msgWrapper = msgWrapper,
						$temp$positionWrapper = positionWrapper,
						$temp$shape = cmbndshp;
					id = $temp$id;
					w = $temp$w;
					h = $temp$h;
					trans = $temp$trans;
					msgWrapper = $temp$msgWrapper;
					positionWrapper = $temp$positionWrapper;
					shape = $temp$shape;
					continue createSVG;
				default:
					var s = shape.a;
					var th = shape.b;
					var c = shape.c;
					return ((th <= 0) || (_Utils_cmp(s, 2 * th) < 0)) ? A2($elm$svg$Svg$g, _List_Nil, _List_Nil) : A7(
						$MacCASOutreach$graphicsvg$GraphicSVG$createSVG,
						id,
						w,
						h,
						trans,
						msgWrapper,
						positionWrapper,
						A4(
							$MacCASOutreach$graphicsvg$GraphicSVG$createGraph,
							_Utils_Tuple2(w, h),
							s,
							th,
							c));
			}
		}
	});
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $MacCASOutreach$graphicsvg$GraphicSVG$createCollage = F3(
	function (w, h, shapes) {
		return A2(
			$elm$svg$Svg$svg,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$width('100%'),
					$elm$svg$Svg$Attributes$height('100%'),
					$elm$svg$Svg$Attributes$style('position:absolute;top:0px;left:0px;'),
					$elm$svg$Svg$Attributes$viewBox(
					$elm$core$String$fromFloat((-w) / 2) + (' ' + ($elm$core$String$fromFloat((-h) / 2) + (' ' + ($elm$core$String$fromFloat(w) + (' ' + $elm$core$String$fromFloat(h))))))),
					$elm$svg$Svg$Attributes$id('render')
				]),
			A2(
				$elm$core$List$cons,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$cPath, w, h),
				_List_fromArray(
					[
						A2(
						$elm$svg$Svg$g,
						_List_fromArray(
							[
								$elm$svg$Svg$Attributes$clipPath('url(#cPath)')
							]),
						A2(
							$elm$core$List$indexedMap,
							function (n) {
								return A6(
									$MacCASOutreach$graphicsvg$GraphicSVG$createSVG,
									$elm$core$String$fromInt(n),
									w,
									h,
									$MacCASOutreach$graphicsvg$GraphicSVG$ident,
									$MacCASOutreach$graphicsvg$GraphicSVG$Graphics,
									$MacCASOutreach$graphicsvg$GraphicSVG$ReturnPosition);
							},
							shapes))
					])));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$hiddenAppView = F2(
	function (userView, _v0) {
		var userModel = _v0.a;
		var userViewEval = userView(userModel);
		var title = userViewEval.db;
		var _v1 = userViewEval.bi;
		var w = _v1.a;
		var h = _v1.b;
		var shapes = _v1.c;
		return {
			bi: _List_fromArray(
				[
					A3($MacCASOutreach$graphicsvg$GraphicSVG$createCollage, w, h, shapes)
				]),
			db: title
		};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$initHiddenModel = {aT: 0, aU: 0, bD: 0, bG: 0};
var $MacCASOutreach$graphicsvg$GraphicSVG$initialCmd = function (userCmd) {
	return $elm$core$Platform$Cmd$batch(
		_List_fromArray(
			[$MacCASOutreach$graphicsvg$GraphicSVG$getViewportSize, userCmd]));
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$core$Platform$Sub$map = _Platform_map;
var $elm$browser$Browser$Events$Window = 1;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {ch: pids, cs: subs};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (!node) {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {b3: event, b7: key};
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (!node) {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.ch,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.b7;
		var event = _v0.b3;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.cs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onResize = function (func) {
	return A3(
		$elm$browser$Browser$Events$on,
		1,
		'resize',
		A2(
			$elm$json$Json$Decode$field,
			'target',
			A3(
				$elm$json$Json$Decode$map2,
				func,
				A2($elm$json$Json$Decode$field, 'innerWidth', $elm$json$Json$Decode$int),
				A2($elm$json$Json$Decode$field, 'innerHeight', $elm$json$Json$Decode$int))));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$subs = F2(
	function (userSubs, _v0) {
		var userModel = _v0.a;
		return $elm$core$Platform$Sub$batch(
			_Utils_ap(
				_List_fromArray(
					[
						$elm$browser$Browser$Events$onResize(
						F2(
							function (_v1, _v2) {
								return $MacCASOutreach$graphicsvg$GraphicSVG$WindowResize($elm$core$Maybe$Nothing);
							}))
					]),
				_List_fromArray(
					[
						A2(
						$elm$core$Platform$Sub$map,
						$MacCASOutreach$graphicsvg$GraphicSVG$Graphics,
						userSubs(userModel))
					])));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$ellieApp = function (input) {
	return $elm$browser$Browser$document(
		{
			a2: function (flags) {
				var userInitCmd = input.a2(flags).b;
				var userInit = input.a2(flags).a;
				var userView = input.dk(userInit).bi;
				var _v0 = userView;
				var initW = _v0.a;
				var initH = _v0.b;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						userInit,
						_Utils_update(
							$MacCASOutreach$graphicsvg$GraphicSVG$initHiddenModel,
							{aT: initH, aU: initW})),
					$MacCASOutreach$graphicsvg$GraphicSVG$initialCmd(
						A2($elm$core$Platform$Cmd$map, $MacCASOutreach$graphicsvg$GraphicSVG$Graphics, userInitCmd)));
			},
			bF: $MacCASOutreach$graphicsvg$GraphicSVG$subs(input.bF),
			dh: A2($MacCASOutreach$graphicsvg$GraphicSVG$hiddenAppUpdate, input.dk, input.dh),
			dk: $MacCASOutreach$graphicsvg$GraphicSVG$hiddenAppView(input.dk)
		});
};
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$DownArrow = {$: 11};
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$Key = function (a) {
	return {$: 0, a: a};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$LeftArrow = {$: 8};
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$RightArrow = {$: 10};
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$UpArrow = {$: 9};
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$WentDown = 1;
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$WentUp = 0;
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$arrowChecker = F5(
	function (checker, up, down, left, right) {
		return _Utils_Tuple2(
			function () {
				var _v0 = _Utils_Tuple2(
					checker(left),
					checker(right));
				_v0$8:
				while (true) {
					switch (_v0.a) {
						case 1:
							switch (_v0.b) {
								case 3:
									var _v1 = _v0.a;
									var _v2 = _v0.b;
									return -1;
								case 2:
									var _v3 = _v0.a;
									var _v4 = _v0.b;
									return -1;
								default:
									break _v0$8;
							}
						case 0:
							switch (_v0.b) {
								case 3:
									var _v5 = _v0.a;
									var _v6 = _v0.b;
									return -1;
								case 2:
									var _v7 = _v0.a;
									var _v8 = _v0.b;
									return -1;
								default:
									break _v0$8;
							}
						case 3:
							switch (_v0.b) {
								case 1:
									var _v9 = _v0.a;
									var _v10 = _v0.b;
									return 1;
								case 0:
									var _v13 = _v0.a;
									var _v14 = _v0.b;
									return 1;
								default:
									break _v0$8;
							}
						default:
							switch (_v0.b) {
								case 1:
									var _v11 = _v0.a;
									var _v12 = _v0.b;
									return 1;
								case 0:
									var _v15 = _v0.a;
									var _v16 = _v0.b;
									return 1;
								default:
									break _v0$8;
							}
					}
				}
				return 0;
			}(),
			function () {
				var _v17 = _Utils_Tuple2(
					checker(down),
					checker(up));
				_v17$8:
				while (true) {
					switch (_v17.a) {
						case 1:
							switch (_v17.b) {
								case 3:
									var _v18 = _v17.a;
									var _v19 = _v17.b;
									return -1;
								case 2:
									var _v20 = _v17.a;
									var _v21 = _v17.b;
									return -1;
								default:
									break _v17$8;
							}
						case 0:
							switch (_v17.b) {
								case 3:
									var _v22 = _v17.a;
									var _v23 = _v17.b;
									return -1;
								case 2:
									var _v24 = _v17.a;
									var _v25 = _v17.b;
									return -1;
								default:
									break _v17$8;
							}
						case 3:
							switch (_v17.b) {
								case 1:
									var _v26 = _v17.a;
									var _v27 = _v17.b;
									return 1;
								case 0:
									var _v30 = _v17.a;
									var _v31 = _v17.b;
									return 1;
								default:
									break _v17$8;
							}
						default:
							switch (_v17.b) {
								case 1:
									var _v28 = _v17.a;
									var _v29 = _v17.b;
									return 1;
								case 0:
									var _v32 = _v17.a;
									var _v33 = _v17.b;
									return 1;
								default:
									break _v17$8;
							}
					}
				}
				return 0;
			}());
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$Down = 1;
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$JustDown = 0;
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$JustUp = 2;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$insertKeyDict = F3(
	function (dict, key, action) {
		var currState = A2($elm$core$Dict$get, key, dict);
		if (!currState.$) {
			if (!currState.a.b) {
				switch (currState.a.a) {
					case 0:
						var _v1 = currState.a;
						var _v2 = _v1.a;
						return A3(
							$elm$core$Dict$insert,
							key,
							function () {
								if (action === 1) {
									return _Utils_Tuple2(0, false);
								} else {
									return _Utils_Tuple2(0, true);
								}
							}(),
							dict);
					case 1:
						var _v4 = currState.a;
						var _v5 = _v4.a;
						return A3(
							$elm$core$Dict$insert,
							key,
							function () {
								if (action === 1) {
									return _Utils_Tuple2(1, false);
								} else {
									return _Utils_Tuple2(2, false);
								}
							}(),
							dict);
					case 3:
						var _v7 = currState.a;
						var _v8 = _v7.a;
						return A3(
							$elm$core$Dict$insert,
							key,
							function () {
								if (action === 1) {
									return _Utils_Tuple2(0, false);
								} else {
									return _Utils_Tuple2(2, false);
								}
							}(),
							dict);
					default:
						var _v10 = currState.a;
						var _v11 = _v10.a;
						return A3(
							$elm$core$Dict$insert,
							key,
							function () {
								if (action === 1) {
									return _Utils_Tuple2(2, true);
								} else {
									return _Utils_Tuple2(2, false);
								}
							}(),
							dict);
				}
			} else {
				var _v13 = currState.a;
				var state = _v13.a;
				return A3(
					$elm$core$Dict$insert,
					key,
					function () {
						if (action === 1) {
							return _Utils_Tuple2(state, true);
						} else {
							return _Utils_Tuple2(state, true);
						}
					}(),
					dict);
			}
		} else {
			return A3(
				$elm$core$Dict$insert,
				key,
				function () {
					if (action === 1) {
						return _Utils_Tuple2(0, false);
					} else {
						return _Utils_Tuple2(2, false);
					}
				}(),
				dict);
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$Up = 3;
var $elm$core$Char$toUpper = _Char_toUpper;
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$keyCheckerFunction = F2(
	function (dict, key) {
		var kc = function () {
			switch (key.$) {
				case 0:
					var str = key.a;
					return $elm$core$Char$toCode(
						$elm$core$Char$toUpper(
							function () {
								var _v10 = $elm$core$String$uncons(str);
								if (!_v10.$) {
									var _v11 = _v10.a;
									var a = _v11.a;
									return a;
								} else {
									return 'z';
								}
							}()));
				case 1:
					return 8;
				case 2:
					return 9;
				case 3:
					return 13;
				case 4:
					return 16;
				case 5:
					return 17;
				case 6:
					return 18;
				case 7:
					return 20;
				case 13:
					return 32;
				case 8:
					return 37;
				case 9:
					return 38;
				case 10:
					return 39;
				case 11:
					return 40;
				default:
					return 46;
			}
		}();
		var state = A2($elm$core$Dict$get, kc, dict);
		if (!state.$) {
			switch (state.a.a) {
				case 0:
					var _v1 = state.a;
					var _v2 = _v1.a;
					return 0;
				case 1:
					var _v3 = state.a;
					var _v4 = _v3.a;
					return 1;
				case 2:
					var _v5 = state.a;
					var _v6 = _v5.a;
					return 2;
				default:
					var _v7 = state.a;
					var _v8 = _v7.a;
					return 3;
			}
		} else {
			return 3;
		}
	});
var $elm$core$Dict$filter = F2(
	function (isGood, dict) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, d) {
					return A2(isGood, k, v) ? A3($elm$core$Dict$insert, k, v, d) : d;
				}),
			$elm$core$Dict$empty,
			dict);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$filterHelper = F2(
	function (key, action) {
		if (action.a === 3) {
			var _v1 = action.a;
			return false;
		} else {
			return true;
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$maintainHelper = F2(
	function (key, action) {
		if (!action.b) {
			switch (action.a) {
				case 2:
					var _v1 = action.a;
					return _Utils_Tuple2(3, false);
				case 3:
					var _v3 = action.a;
					return _Utils_Tuple2(3, false);
				case 0:
					var _v5 = action.a;
					return _Utils_Tuple2(1, false);
				default:
					var _v7 = action.a;
					return _Utils_Tuple2(1, false);
			}
		} else {
			switch (action.a) {
				case 2:
					var _v2 = action.a;
					return _Utils_Tuple2(0, false);
				case 3:
					var _v4 = action.a;
					return _Utils_Tuple2(3, false);
				case 0:
					var _v6 = action.a;
					return _Utils_Tuple2(2, false);
				default:
					var _v8 = action.a;
					return _Utils_Tuple2(1, false);
			}
		}
	});
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$maintainKeyDict = function (dict) {
	return A2(
		$elm$core$Dict$filter,
		$MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$filterHelper,
		A2($elm$core$Dict$map, $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$maintainHelper, dict));
};
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0;
	return millis;
};
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$subtractTimeSeconds = F2(
	function (t1, t0) {
		return ($elm$time$Time$posixToMillis(t1) - $elm$time$Time$posixToMillis(t0)) / 1000;
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$hiddenGameUpdate = F3(
	function (userUpdate, msg, _v0) {
		var userModel = _v0.a;
		var hiddenModel = _v0.b;
		var updateTick = hiddenModel.bd;
		switch (msg.$) {
			case 0:
				var userMsg = msg.a;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						A2(userUpdate, userMsg, userModel),
						hiddenModel),
					$elm$core$Platform$Cmd$none);
			case 1:
				var t = msg.a;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						userModel,
						_Utils_update(
							hiddenModel,
							{a3: t})),
					$elm$core$Platform$Cmd$none);
			case 2:
				var t = msg.a;
				var timeInSeconds = A2($MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$subtractTimeSeconds, t, hiddenModel.a3);
				var keyChecker = $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$keyCheckerFunction(hiddenModel.B);
				var wasd = A5(
					$MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$arrowChecker,
					keyChecker,
					$MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$Key('w'),
					$MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$Key('s'),
					$MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$Key('a'),
					$MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$Key('d'));
				var arrowKeys = A5($MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$arrowChecker, keyChecker, $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$UpArrow, $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$DownArrow, $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$LeftArrow, $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$RightArrow);
				var newModel = A2(
					userUpdate,
					A2(
						hiddenModel.bd,
						timeInSeconds,
						_Utils_Tuple3(keyChecker, arrowKeys, wasd)),
					userModel);
				return _Utils_Tuple2(
					_Utils_Tuple2(
						newModel,
						_Utils_update(
							hiddenModel,
							{
								B: $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$maintainKeyDict(hiddenModel.B)
							})),
					$elm$core$Platform$Cmd$none);
			case 3:
				var keyCode = msg.a;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						userModel,
						_Utils_update(
							hiddenModel,
							{
								B: A3($MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$insertKeyDict, hiddenModel.B, keyCode, 1)
							})),
					$elm$core$Platform$Cmd$none);
			default:
				var keyCode = msg.a;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						userModel,
						_Utils_update(
							hiddenModel,
							{
								B: A3($MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$insertKeyDict, hiddenModel.B, keyCode, 0)
							})),
					$elm$core$Platform$Cmd$none);
		}
	});
var $elm$time$Time$Posix = $elm$core$Basics$identity;
var $elm$time$Time$millisToPosix = $elm$core$Basics$identity;
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$initHiddenModel = function (tick) {
	return {
		a3: $elm$time$Time$millisToPosix(0),
		B: $elm$core$Dict$empty,
		bd: tick
	};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$Collage = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$map = F2(
	function (f, sh) {
		switch (sh.$) {
			case 0:
				var fillClr = sh.a;
				var lt = sh.b;
				var stencil = sh.c;
				return A3($MacCASOutreach$graphicsvg$GraphicSVG$Inked, fillClr, lt, stencil);
			case 1:
				var w = sh.a;
				var h = sh.b;
				var htm = sh.c;
				return A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$ForeignObject,
					w,
					h,
					A2($elm$html$Html$map, f, htm));
			case 2:
				var v = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Move,
					v,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 3:
				var deg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Rotate,
					deg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 4:
				var sx = sh.a;
				var sy = sh.b;
				var shape = sh.c;
				return A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$Scale,
					sx,
					sy,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 5:
				var skx = sh.a;
				var sky = sh.b;
				var shape = sh.c;
				return A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$Skew,
					skx,
					sky,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 6:
				var tm = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Transformed,
					tm,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 13:
				var href = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Link,
					href,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 9:
				var sh1 = sh.a;
				var sh2 = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$AlphaMask,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, sh1),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, sh2));
			case 10:
				var sh1 = sh.a;
				var sh2 = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Clip,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, sh1),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, sh2));
			case 11:
				return $MacCASOutreach$graphicsvg$GraphicSVG$Everything;
			case 12:
				return $MacCASOutreach$graphicsvg$GraphicSVG$Notathing;
			case 14:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Tap,
					f(msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 15:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TapAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 16:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$EnterShape,
					f(msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 17:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$EnterAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 18:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Exit,
					f(msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 19:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$ExitAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 20:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$MouseDown,
					f(msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 21:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$MouseDownAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 22:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$MouseUp,
					f(msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 23:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$MouseUpAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 24:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$MoveOverAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 25:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TouchStart,
					f(msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 26:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TouchEnd,
					f(msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 27:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TouchStartAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 28:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TouchEndAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 29:
				var msg = sh.a;
				var shape = sh.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TouchMoveAt,
					A2($elm$core$Basics$composeL, f, msg),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, shape));
			case 7:
				var shapes = sh.a;
				return $MacCASOutreach$graphicsvg$GraphicSVG$Group(
					A2(
						$elm$core$List$map,
						$MacCASOutreach$graphicsvg$GraphicSVG$map(f),
						shapes));
			case 8:
				var cmbndshp = sh.a;
				return $MacCASOutreach$graphicsvg$GraphicSVG$GroupOutline(
					A2($MacCASOutreach$graphicsvg$GraphicSVG$map, f, cmbndshp));
			default:
				var s = sh.a;
				var th = sh.b;
				var c = sh.c;
				return A3($MacCASOutreach$graphicsvg$GraphicSVG$GraphPaper, s, th, c);
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$mapCollage = F2(
	function (f, _v0) {
		var w = _v0.a;
		var h = _v0.b;
		var shapes = _v0.c;
		return A3(
			$MacCASOutreach$graphicsvg$GraphicSVG$Collage,
			w,
			h,
			A2(
				$elm$core$List$map,
				$MacCASOutreach$graphicsvg$GraphicSVG$map(f),
				shapes));
	});
var $elm$time$Time$Name = function (a) {
	return {$: 0, a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 1, a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$KeyDown = function (a) {
	return {$: 3, a: a};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$KeyUp = function (a) {
	return {$: 4, a: a};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$TickTime = function (a) {
	return {$: 2, a: a};
};
var $elm$browser$Browser$AnimationManager$Time = function (a) {
	return {$: 0, a: a};
};
var $elm$browser$Browser$AnimationManager$State = F3(
	function (subs, request, oldTime) {
		return {bQ: oldTime, co: request, cs: subs};
	});
var $elm$browser$Browser$AnimationManager$init = $elm$core$Task$succeed(
	A3($elm$browser$Browser$AnimationManager$State, _List_Nil, $elm$core$Maybe$Nothing, 0));
var $elm$browser$Browser$AnimationManager$now = _Browser_now(0);
var $elm$browser$Browser$AnimationManager$rAF = _Browser_rAF(0);
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$browser$Browser$AnimationManager$onEffects = F3(
	function (router, subs, _v0) {
		var request = _v0.co;
		var oldTime = _v0.bQ;
		var _v1 = _Utils_Tuple2(request, subs);
		if (_v1.a.$ === 1) {
			if (!_v1.b.b) {
				var _v2 = _v1.a;
				return $elm$browser$Browser$AnimationManager$init;
			} else {
				var _v4 = _v1.a;
				return A2(
					$elm$core$Task$andThen,
					function (pid) {
						return A2(
							$elm$core$Task$andThen,
							function (time) {
								return $elm$core$Task$succeed(
									A3(
										$elm$browser$Browser$AnimationManager$State,
										subs,
										$elm$core$Maybe$Just(pid),
										time));
							},
							$elm$browser$Browser$AnimationManager$now);
					},
					$elm$core$Process$spawn(
						A2(
							$elm$core$Task$andThen,
							$elm$core$Platform$sendToSelf(router),
							$elm$browser$Browser$AnimationManager$rAF)));
			}
		} else {
			if (!_v1.b.b) {
				var pid = _v1.a.a;
				return A2(
					$elm$core$Task$andThen,
					function (_v3) {
						return $elm$browser$Browser$AnimationManager$init;
					},
					$elm$core$Process$kill(pid));
			} else {
				return $elm$core$Task$succeed(
					A3($elm$browser$Browser$AnimationManager$State, subs, request, oldTime));
			}
		}
	});
var $elm$browser$Browser$AnimationManager$onSelfMsg = F3(
	function (router, newTime, _v0) {
		var subs = _v0.cs;
		var oldTime = _v0.bQ;
		var send = function (sub) {
			if (!sub.$) {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(
						$elm$time$Time$millisToPosix(newTime)));
			} else {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(newTime - oldTime));
			}
		};
		return A2(
			$elm$core$Task$andThen,
			function (pid) {
				return A2(
					$elm$core$Task$andThen,
					function (_v1) {
						return $elm$core$Task$succeed(
							A3(
								$elm$browser$Browser$AnimationManager$State,
								subs,
								$elm$core$Maybe$Just(pid),
								newTime));
					},
					$elm$core$Task$sequence(
						A2($elm$core$List$map, send, subs)));
			},
			$elm$core$Process$spawn(
				A2(
					$elm$core$Task$andThen,
					$elm$core$Platform$sendToSelf(router),
					$elm$browser$Browser$AnimationManager$rAF)));
	});
var $elm$browser$Browser$AnimationManager$Delta = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$AnimationManager$subMap = F2(
	function (func, sub) {
		if (!sub.$) {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Time(
				A2($elm$core$Basics$composeL, func, tagger));
		} else {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Delta(
				A2($elm$core$Basics$composeL, func, tagger));
		}
	});
_Platform_effectManagers['Browser.AnimationManager'] = _Platform_createManager($elm$browser$Browser$AnimationManager$init, $elm$browser$Browser$AnimationManager$onEffects, $elm$browser$Browser$AnimationManager$onSelfMsg, 0, $elm$browser$Browser$AnimationManager$subMap);
var $elm$browser$Browser$AnimationManager$subscription = _Platform_leaf('Browser.AnimationManager');
var $elm$browser$Browser$AnimationManager$onAnimationFrame = function (tagger) {
	return $elm$browser$Browser$AnimationManager$subscription(
		$elm$browser$Browser$AnimationManager$Time(tagger));
};
var $elm$browser$Browser$Events$onAnimationFrame = $elm$browser$Browser$AnimationManager$onAnimationFrame;
var $elm$browser$Browser$Events$Document = 0;
var $elm$browser$Browser$Events$onKeyDown = A2($elm$browser$Browser$Events$on, 0, 'keydown');
var $elm$browser$Browser$Events$onKeyUp = A2($elm$browser$Browser$Events$on, 0, 'keyup');
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$subs = _List_fromArray(
	[
		$elm$browser$Browser$Events$onKeyUp(
		A2(
			$elm$json$Json$Decode$map,
			$MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$KeyUp,
			A2($elm$json$Json$Decode$field, 'keyCode', $elm$json$Json$Decode$int))),
		$elm$browser$Browser$Events$onKeyDown(
		A2(
			$elm$json$Json$Decode$map,
			$MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$KeyDown,
			A2($elm$json$Json$Decode$field, 'keyCode', $elm$json$Json$Decode$int))),
		$elm$browser$Browser$Events$onAnimationFrame($MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$TickTime)
	]);
var $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$gameApp = F2(
	function (tickMsg, userApp) {
		var userView = userApp.dk;
		var userUpdate = userApp.dh;
		var userInit = userApp.cY;
		return $MacCASOutreach$graphicsvg$GraphicSVG$ellieApp(
			{
				a2: function (_v0) {
					return _Utils_Tuple2(
						_Utils_Tuple2(
							userInit,
							$MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$initHiddenModel(tickMsg)),
						A2($elm$core$Task$perform, $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$InitTime, $elm$time$Time$now));
				},
				bF: function (_v1) {
					return $elm$core$Platform$Sub$batch($MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$subs);
				},
				dh: $MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$hiddenGameUpdate(userUpdate),
				dk: function (_v2) {
					var userModel = _v2.a;
					return {
						bi: A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$mapCollage,
							$MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$UserMsg,
							userView(userModel)),
						db: userApp.db
					};
				}
			});
	});
var $author$project$Main$ShapeCreator = 0;
var $author$project$ArcCreator$BezierPath = 0;
var $author$project$ArcCreator$Filled = 0;
var $author$project$ArcCreator$None = 4;
var $author$project$ArcCreator$NotifyTap = 0;
var $author$project$ArcCreator$RGB = 0;
var $author$project$ArcCreator$Solid = 0;
var $author$project$ArcCreator$init = {
	ad: 0.5,
	aI: 30,
	cA: true,
	C: 0,
	l: 0,
	bj: 0,
	M: 4,
	N: 0,
	O: 0,
	aV: 0,
	cH: 1,
	D: 150,
	bm: true,
	aZ: false,
	b5: false,
	a_: false,
	a$: false,
	a0: false,
	a1: false,
	cS: 15,
	cW: 0,
	I: 1,
	bp: _Utils_Tuple2(-201, 176),
	H: 0,
	cZ: 0,
	c_: 0.75,
	cc: 0,
	c7: 0,
	t: _List_fromArray(
		[
			_Utils_Tuple2(
			_Utils_Tuple2(0, 0),
			_Utils_Tuple2(0, 0))
		]),
	G: 250,
	c9: 5,
	_: 2,
	aB: 2,
	aC: 2,
	bT: 0,
	da: 5,
	br: 0,
	aO: 0,
	dl: 10,
	cv: 5,
	bg: 0
};
var $author$project$PolygonCreator$Filled = 0;
var $author$project$PolygonCreator$No = 4;
var $author$project$PolygonCreator$None = 6;
var $author$project$PolygonCreator$NotifyTap = 0;
var $author$project$PolygonCreator$RGB = 0;
var $author$project$PolygonCreator$Solid = 0;
var $author$project$PolygonCreator$Triangle = 5;
var $author$project$PolygonCreator$init = {
	ad: 0.5,
	aI: 60,
	C: 255,
	l: 0,
	bj: 0,
	M: 6,
	j: -1,
	aV: 0,
	by: 1,
	D: 182,
	aZ: false,
	b5: false,
	cI: true,
	cJ: false,
	cK: true,
	cL: false,
	cM: false,
	cN: false,
	cO: false,
	cP: false,
	cQ: false,
	cR: false,
	bA: true,
	bB: true,
	a_: false,
	a$: false,
	a0: false,
	a1: false,
	bn: false,
	cS: 5,
	I: 1,
	cc: 0,
	z: 0,
	R: _Utils_Tuple2(0, 0),
	F: _Utils_Tuple2(0, 0),
	af: _Utils_Tuple2(0, 0),
	ag: _Utils_Tuple2(0, 0),
	ah: _Utils_Tuple2(0, 0),
	ai: _Utils_Tuple2(0, 0),
	aj: _Utils_Tuple2(0, 0),
	ak: _Utils_Tuple2(0, 0),
	al: _Utils_Tuple2(0, 0),
	am: _Utils_Tuple2(0, 0),
	c: _List_fromArray(
		[
			_Utils_Tuple2(0, 0)
		]),
	G: 0,
	c9: 5,
	_: 2,
	aB: 2,
	aC: 2,
	bT: 5,
	cq: 50,
	cr: 50,
	da: 5,
	br: 0,
	K: _List_fromArray(
		[true, false, false, false, false, false, false, false, false, true]),
	aO: 0,
	dc: false,
	dd: false,
	de: false,
	A: 4,
	dl: 5,
	cv: 50,
	bg: 50
};
var $author$project$ShapeCreator$Circle = 0;
var $author$project$ShapeCreator$Filled = 0;
var $author$project$ShapeCreator$None = 6;
var $author$project$ShapeCreator$NotifyTap = 0;
var $author$project$ShapeCreator$RGB = 35;
var $author$project$ShapeCreator$Solid = 0;
var $author$project$ShapeCreator$init = {ad: 0.5, aI: 30, C: 50, l: 0, bj: 35, M: 6, aV: 0, D: 125, aZ: false, b5: false, a_: false, a$: false, a0: false, a1: false, cS: 15, I: 1, c_: 0.75, cc: 0, G: 230, c9: 5, _: 2, aB: 2, aC: 2, bT: 0, da: 5, br: 0, aO: 0, aP: 'Hello', dl: 10, cv: 50, bg: 50};
var $author$project$SinCreator$None = 16;
var $author$project$SinCreator$NotifyTap = 0;
var $author$project$SinCreator$OneFun = 0;
var $author$project$SinCreator$ScaleU = 0;
var $author$project$SinCreator$Sin = 0;
var $author$project$SinCreator$UFun = 1;
var $author$project$SinCreator$UFunZero = 1;
var $author$project$SinCreator$VFun = 2;
var $author$project$SinCreator$ZeroFun = 0;
var $elm$core$Basics$pi = _Basics_pi;
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$ssc = function (n) {
	return A3($elm$core$Basics$clamp, 0, 255, n);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$rgb = F3(
	function (r, g, b) {
		return A4(
			$MacCASOutreach$graphicsvg$GraphicSVG$RGBA,
			$MacCASOutreach$graphicsvg$GraphicSVG$ssc(r),
			$MacCASOutreach$graphicsvg$GraphicSVG$ssc(g),
			$MacCASOutreach$graphicsvg$GraphicSVG$ssc(b),
			1);
	});
var $author$project$SinCreator$init = {
	cx: 0.25,
	aR: 0,
	aS: 1,
	n: 1,
	bx: 0.25,
	l: 0,
	bk: _List_Nil,
	b_: 200,
	M: 16,
	b$: 0,
	b1: 0,
	o: 0,
	m: 0,
	bl: 0,
	cE: 0,
	aX: 0,
	aY: 2,
	p: 1,
	bz: 0.25,
	cV: _Utils_Tuple3(
		0,
		0,
		A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 160, 128, 96)),
	r: 20,
	x: 10,
	cX: 2 * $elm$core$Basics$pi,
	c$: 0.25,
	c0: 0.25,
	c1: 0.25,
	c2: 0.25,
	c3: 0,
	b9: 1,
	c4: 1,
	ca: 0,
	cc: 0,
	ba: 0,
	bb: 0,
	q: 1,
	bC: 0.25,
	bq: _List_Nil,
	bE: 120,
	aO: $elm$core$Maybe$Nothing,
	df: 0,
	bH: 0.25,
	bI: 0.25,
	aD: 0,
	bJ: 0,
	bK: 1,
	aE: 0,
	be: 0,
	k: 1,
	b: 5,
	L: 0,
	bv: 1,
	aF: 0,
	dg: 0.5,
	ac: 0,
	bW: 1,
	bX: 0,
	aH: 1,
	w: 5,
	di: 0.5,
	dj: 0.5
};
var $author$project$TextCreator$Filled = 0;
var $author$project$TextCreator$None = 7;
var $author$project$TextCreator$NotifyTap = 0;
var $author$project$TextCreator$RGB = 0;
var $author$project$TextCreator$Solid = 0;
var $author$project$TextCreator$Text = 8;
var $author$project$TextCreator$init = {ad: 0.5, aI: 30, C: 100, l: 0, bj: 0, M: 7, bN: 0, aV: 0, D: 0, aq: false, ar: false, as: false, at: false, aZ: false, b5: false, a_: false, au: false, a$: false, a0: false, a1: false, av: false, ae: false, aw: false, ax: false, ay: false, cS: 15, aL: 1, I: 1, c_: 0.75, bP: 0, cc: 0, G: 100, c9: 5, _: 2, aB: 2, aC: 2, bT: 8, da: 5, br: 0, aO: 0, aP: '', an: 14, dl: 10, cv: 0, bg: 0};
var $author$project$TriCreator$Filled = 0;
var $author$project$TriCreator$None = 36;
var $author$project$TriCreator$NotifyTap = 0;
var $author$project$TriCreator$RGB = 0;
var $author$project$TriCreator$Solid = 0;
var $author$project$TriCreator$Triangle = 1;
var $elm$core$Basics$degrees = function (angleInDegrees) {
	return (angleInDegrees * $elm$core$Basics$pi) / 180;
};
var $author$project$TriCreator$init = {
	ad: 0.5,
	aI: 30,
	S: $elm$core$Basics$degrees(30),
	cy: $elm$core$Basics$degrees(40),
	C: 100,
	l: 0,
	bj: 0,
	M: 36,
	aV: 0,
	D: 0,
	aZ: false,
	b5: false,
	a_: false,
	a$: false,
	a0: false,
	a1: false,
	cS: 15,
	I: 1,
	cc: 0,
	U: 0,
	V: 0,
	W: 40,
	X: 10,
	Y: -20,
	Z: 30,
	G: 100,
	_: 2,
	aB: 2,
	aC: 2,
	bT: 1,
	br: 0,
	aO: 0,
	dl: 10,
	cv: 0,
	bg: 0
};
var $author$project$Main$init = {cC: 1, e: 0, f: 0, a4: $author$project$ShapeCreator$init, a5: $author$project$TriCreator$init, a6: $author$project$PolygonCreator$init, a7: $author$project$ArcCreator$init, a8: $author$project$SinCreator$init, a9: $author$project$TextCreator$init, g: 1, a: 0, d: 0, h: 0, i: 0};
var $author$project$Main$ArcCreator = 3;
var $author$project$Main$PolygonCreator = 2;
var $author$project$Main$SinCreator = 4;
var $author$project$Main$TextCreator = 5;
var $author$project$ArcCreator$Tick = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$PolygonCreator$Tick = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$ShapeCreator$Tick = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$SinCreator$Tick = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$TextCreator$Tick = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$TriCreator$Tick = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Main$TriCreator = 1;
var $author$project$ArcCreator$Dashed = 2;
var $author$project$ArcCreator$Dotdash = 4;
var $author$project$ArcCreator$Dotted = 1;
var $author$project$ArcCreator$Longdash = 3;
var $elm$core$Basics$pow = _Basics_pow;
var $author$project$ArcCreator$curveMovingFunction = function (x) {
	return $elm$core$Basics$round(
		A3(
			$elm$core$Basics$clamp,
			0,
			12,
			A2($elm$core$Basics$pow, x, 2)) / 4);
};
var $author$project$ArcCreator$movePoint = F2(
	function (_v0, d) {
		var x = _v0.a;
		var y = _v0.b;
		switch (d) {
			case 2:
				return _Utils_Tuple2(x - 1, y);
			case 3:
				return _Utils_Tuple2(x + 1, y);
			case 0:
				return _Utils_Tuple2(x, y + 1);
			default:
				return _Utils_Tuple2(x, y - 1);
		}
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$ArcCreator$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var t = msg.a;
				return _Utils_update(
					model,
					{
						C: function () {
							var _v1 = model.H;
							var _v2 = model.M;
							switch (_v2) {
								case 9:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.C + $author$project$ArcCreator$curveMovingFunction(model.l));
								case 10:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.C - $author$project$ArcCreator$curveMovingFunction(model.l));
								default:
									return model.C;
							}
						}(),
						l: function () {
							var _v3 = model.M;
							if (_v3 === 4) {
								return 0;
							} else {
								return model.l + 0.1;
							}
						}(),
						N: function () {
							var _v4 = model.H;
							if (!_v4) {
								var _v5 = model.M;
								switch (_v5) {
									case 3:
										return model.N + $author$project$ArcCreator$curveMovingFunction(model.l);
									case 2:
										return model.N - $author$project$ArcCreator$curveMovingFunction(model.l);
									default:
										return model.N;
								}
							} else {
								return model.N;
							}
						}(),
						O: function () {
							var _v6 = model.H;
							if (!_v6) {
								var _v7 = model.M;
								switch (_v7) {
									case 0:
										return model.O + $author$project$ArcCreator$curveMovingFunction(model.l);
									case 1:
										return model.O - $author$project$ArcCreator$curveMovingFunction(model.l);
									default:
										return model.O;
								}
							} else {
								return model.O;
							}
						}(),
						D: function () {
							var _v8 = model.H;
							var _v9 = model.M;
							switch (_v9) {
								case 7:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.D + $author$project$ArcCreator$curveMovingFunction(model.l));
								case 8:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.D - $author$project$ArcCreator$curveMovingFunction(model.l));
								default:
									return model.D;
							}
						}(),
						t: function () {
							var replacePull = F3(
								function (target, current, pulls) {
									if (pulls.b) {
										var _v11 = pulls.a;
										var _v12 = _v11.a;
										var x1 = _v12.a;
										var y1 = _v12.b;
										var _v13 = _v11.b;
										var x2 = _v13.a;
										var y2 = _v13.b;
										var rest = pulls.b;
										return _Utils_eq(target, current) ? A2(
											$elm$core$List$cons,
											_Utils_Tuple2(
												_Utils_Tuple2(
													function () {
														var _v14 = model.M;
														switch (_v14) {
															case 3:
																return x1 + $author$project$ArcCreator$curveMovingFunction(model.l);
															case 2:
																return x1 - $author$project$ArcCreator$curveMovingFunction(model.l);
															default:
																return x1;
														}
													}(),
													function () {
														var _v15 = model.M;
														switch (_v15) {
															case 0:
																return y1 + $author$project$ArcCreator$curveMovingFunction(model.l);
															case 1:
																return y1 - $author$project$ArcCreator$curveMovingFunction(model.l);
															default:
																return y1;
														}
													}()),
												_Utils_Tuple2(x2, y2)),
											rest) : (_Utils_eq(target, current + 1) ? A2(
											$elm$core$List$cons,
											_Utils_Tuple2(
												_Utils_Tuple2(x1, y1),
												_Utils_Tuple2(
													function () {
														var _v16 = model.M;
														switch (_v16) {
															case 3:
																return x2 + $author$project$ArcCreator$curveMovingFunction(model.l);
															case 2:
																return x2 - $author$project$ArcCreator$curveMovingFunction(model.l);
															default:
																return x2;
														}
													}(),
													function () {
														var _v17 = model.M;
														switch (_v17) {
															case 0:
																return y2 + $author$project$ArcCreator$curveMovingFunction(model.l);
															case 1:
																return y2 - $author$project$ArcCreator$curveMovingFunction(model.l);
															default:
																return y2;
														}
													}())),
											rest) : A2(
											$elm$core$List$cons,
											_Utils_Tuple2(
												_Utils_Tuple2(x1, y1),
												_Utils_Tuple2(x2, y2)),
											A3(replacePull, target, current + 2, rest)));
									} else {
										return _List_Nil;
									}
								});
							return A3(replacePull, model.H, 1, model.t);
						}(),
						G: function () {
							var _v18 = model.H;
							var _v19 = model.M;
							switch (_v19) {
								case 5:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.G + $author$project$ArcCreator$curveMovingFunction(model.l));
								case 6:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.G - $author$project$ArcCreator$curveMovingFunction(model.l));
								default:
									return model.G;
							}
						}(),
						aO: t
					});
			case 11:
				var dir = msg.a;
				return _Utils_update(
					model,
					{M: dir});
			case 1:
				var _v20 = msg.a;
				return _Utils_update(
					model,
					{bT: 0});
			case 2:
				var draw = msg.a;
				return _Utils_update(
					model,
					{aV: draw});
			case 3:
				return _Utils_update(
					model,
					{
						br: function () {
							var _v21 = model.br;
							switch (_v21) {
								case 0:
									return 1;
								case 1:
									return 2;
								case 2:
									return 3;
								case 3:
									return 4;
								default:
									return 0;
							}
						}()
					});
			case 5:
				switch (msg.a) {
					case 0:
						var _v22 = msg.a;
						return _Utils_update(
							model,
							{a_: !model.a_});
					case 1:
						var _v23 = msg.a;
						return _Utils_update(
							model,
							{a$: !model.a$});
					case 2:
						var _v24 = msg.a;
						return _Utils_update(
							model,
							{a0: !model.a0});
					case 3:
						var _v25 = msg.a;
						return _Utils_update(
							model,
							{a1: !model.a1});
					case 4:
						var _v26 = msg.a;
						return _Utils_update(
							model,
							{aZ: !model.aZ});
					default:
						var _v27 = msg.a;
						return _Utils_update(
							model,
							{bm: !model.bm});
				}
			case 9:
				switch (msg.a) {
					case 0:
						var _v28 = msg.a;
						return _Utils_update(
							model,
							{bg: 0});
					case 1:
						var _v29 = msg.a;
						return _Utils_update(
							model,
							{bg: 0});
					case 2:
						var _v30 = msg.a;
						return _Utils_update(
							model,
							{bg: 0});
					default:
						var _v31 = msg.a;
						return _Utils_update(
							model,
							{bg: 0});
				}
			case 6:
				var t = msg.a;
				return t(model);
			case 4:
				var clr = msg.a;
				return _Utils_update(
					model,
					{bj: clr});
			case 8:
				var t = msg.a;
				return t(model);
			case 7:
				var notif = msg.a;
				return _Utils_update(
					model,
					{cc: notif});
			default:
				var direct = msg.a;
				var replacePull = F3(
					function (target, current, pulls) {
						if (pulls.b) {
							var _v33 = pulls.a;
							var _v34 = _v33.a;
							var x1 = _v34.a;
							var y1 = _v34.b;
							var _v35 = _v33.b;
							var x2 = _v35.a;
							var y2 = _v35.b;
							var rest = pulls.b;
							return _Utils_eq(target, current) ? A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									A2(
										$author$project$ArcCreator$movePoint,
										_Utils_Tuple2(x1, y1),
										direct),
									_Utils_Tuple2(x2, y2)),
								rest) : (_Utils_eq(target, current + 1) ? A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									_Utils_Tuple2(x1, y1),
									A2(
										$author$project$ArcCreator$movePoint,
										_Utils_Tuple2(x2, y2),
										direct)),
								rest) : A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									_Utils_Tuple2(x1, y1),
									_Utils_Tuple2(x2, y2)),
								A3(replacePull, target, current + 2, rest)));
						} else {
							return _List_Nil;
						}
					});
				var _v36 = A2(
					$author$project$ArcCreator$movePoint,
					_Utils_Tuple2(model.N, model.O),
					direct);
				var cx1 = _v36.a;
				var cy1 = _v36.b;
				return (!(!model.H)) ? _Utils_update(
					model,
					{
						t: A3(replacePull, model.H, 1, model.t)
					}) : _Utils_update(
					model,
					{N: cx1, O: cy1});
		}
	});
var $author$project$PolygonCreator$Dashed = 2;
var $author$project$PolygonCreator$Dotdash = 4;
var $author$project$PolygonCreator$Dotted = 1;
var $author$project$PolygonCreator$Down = 1;
var $author$project$PolygonCreator$Left = 2;
var $author$project$PolygonCreator$Longdash = 3;
var $author$project$PolygonCreator$Right = 3;
var $author$project$PolygonCreator$Up = 0;
var $author$project$PolygonCreator$accel = function (x) {
	return $elm$core$Basics$round(
		A3(
			$elm$core$Basics$clamp,
			0,
			12,
			A2($elm$core$Basics$pow, x, 2)) / 4);
};
var $author$project$PolygonCreator$movePoint = F2(
	function (m, d) {
		switch (d) {
			case 2:
				return function (_v1) {
					var x = _v1.a;
					var y = _v1.b;
					return _Utils_Tuple2(x - 1, y);
				}(m);
			case 3:
				return function (_v2) {
					var x = _v2.a;
					var y = _v2.b;
					return _Utils_Tuple2(x + 1, y);
				}(m);
			case 0:
				return function (_v3) {
					var x = _v3.a;
					var y = _v3.b;
					return _Utils_Tuple2(x, y + 1);
				}(m);
			case 1:
				return function (_v4) {
					var x = _v4.a;
					var y = _v4.b;
					return _Utils_Tuple2(x, y - 1);
				}(m);
			default:
				return m;
		}
	});
var $author$project$PolygonCreator$clickHold = F4(
	function (m, p, pointVal, direct) {
		return _Utils_eq(m.j, p) ? A2($author$project$PolygonCreator$movePoint, pointVal, direct) : pointVal;
	});
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{y: nodeList, s: nodeListSize, v: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$setHelp = F4(
	function (shift, index, value, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
		if (!_v0.$) {
			var subTree = _v0.a;
			var newSub = A4($elm$core$Array$setHelp, shift - $elm$core$Array$shiftStep, index, value, subTree);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$SubTree(newSub),
				tree);
		} else {
			var values = _v0.a;
			var newLeaf = A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, values);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$Leaf(newLeaf),
				tree);
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$set = F3(
	function (index, value, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? array : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			tree,
			A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, tail)) : A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A4($elm$core$Array$setHelp, startShift, index, value, tree),
			tail));
	});
var $author$project$PolygonCreator$setPoint = function (m) {
	return _Utils_ap(
		m.c,
		_List_fromArray(
			[
				_Utils_Tuple2(m.cv, m.bg)
			]));
};
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $author$project$PolygonCreator$this = F4(
	function (m, p, pointVal, dir) {
		return (m.j !== 10) ? $elm$core$Array$toList(
			A3(
				$elm$core$Array$set,
				m.j - 1,
				A4($author$project$PolygonCreator$clickHold, m, p, pointVal, dir),
				$elm$core$Array$fromList(m.c))) : m.c;
	});
var $author$project$PolygonCreator$whichPoint = function (m) {
	var _v0 = m.j;
	switch (_v0) {
		case 1:
			return m.R;
		case 2:
			return m.af;
		case 3:
			return m.ag;
		case 4:
			return m.ah;
		case 5:
			return m.ai;
		case 6:
			return m.aj;
		case 7:
			return m.ak;
		case 8:
			return m.al;
		case 9:
			return m.am;
		default:
			return m.F;
	}
};
var $author$project$PolygonCreator$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var t = msg.a;
				return _Utils_update(
					model,
					{
						C: function () {
							var _v1 = model.M;
							switch (_v1) {
								case 4:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.C + $author$project$PolygonCreator$accel(model.l));
								case 5:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.C - $author$project$PolygonCreator$accel(model.l));
								default:
									return model.C;
							}
						}(),
						l: function () {
							var _v2 = model.M;
							if (_v2 === 6) {
								return 0;
							} else {
								return model.l + 0.1;
							}
						}(),
						D: function () {
							var _v3 = model.M;
							switch (_v3) {
								case 2:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.D + $author$project$PolygonCreator$accel(model.l));
								case 3:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.D - $author$project$PolygonCreator$accel(model.l));
								default:
									return model.D;
							}
						}(),
						R: A4($author$project$PolygonCreator$clickHold, model, 1, model.R, model.A),
						F: A4($author$project$PolygonCreator$clickHold, model, 10, model.F, model.A),
						af: A4($author$project$PolygonCreator$clickHold, model, 2, model.af, model.A),
						ag: A4($author$project$PolygonCreator$clickHold, model, 3, model.ag, model.A),
						ah: A4($author$project$PolygonCreator$clickHold, model, 4, model.ah, model.A),
						ai: A4($author$project$PolygonCreator$clickHold, model, 5, model.ai, model.A),
						aj: A4($author$project$PolygonCreator$clickHold, model, 6, model.aj, model.A),
						ak: A4($author$project$PolygonCreator$clickHold, model, 7, model.ak, model.A),
						al: A4($author$project$PolygonCreator$clickHold, model, 8, model.al, model.A),
						am: A4($author$project$PolygonCreator$clickHold, model, 9, model.am, model.A),
						c: A4(
							$author$project$PolygonCreator$this,
							model,
							model.j,
							$author$project$PolygonCreator$whichPoint(model),
							model.A),
						G: function () {
							var _v4 = model.M;
							switch (_v4) {
								case 0:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.G + $author$project$PolygonCreator$accel(model.l));
								case 1:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.G - $author$project$PolygonCreator$accel(model.l));
								default:
									return model.G;
							}
						}(),
						aO: t
					});
			case 1:
				var draw = msg.a;
				return _Utils_update(
					model,
					{aV: draw});
			case 2:
				return _Utils_update(
					model,
					{
						br: function () {
							var _v5 = model.br;
							switch (_v5) {
								case 0:
									return 1;
								case 1:
									return 2;
								case 2:
									return 3;
								case 3:
									return 4;
								default:
									return 0;
							}
						}()
					});
			case 4:
				switch (msg.a) {
					case 0:
						var _v6 = msg.a;
						return _Utils_update(
							model,
							{b5: !model.b5});
					case 1:
						var _v7 = msg.a;
						return _Utils_update(
							model,
							{a_: !model.a_});
					case 2:
						var _v8 = msg.a;
						return _Utils_update(
							model,
							{a$: !model.a$});
					case 3:
						var _v9 = msg.a;
						return _Utils_update(
							model,
							{a0: !model.a0});
					case 4:
						var _v10 = msg.a;
						return _Utils_update(
							model,
							{a1: !model.a1});
					default:
						var _v11 = msg.a;
						return _Utils_update(
							model,
							{aZ: !model.aZ});
				}
			case 5:
				var t = msg.a;
				return t(model);
			case 3:
				var clr = msg.a;
				return _Utils_update(
					model,
					{bj: clr});
			case 6:
				var notif = msg.a;
				return _Utils_update(
					model,
					{cc: notif});
			case 7:
				var direct = msg.a;
				var _v12 = model.j;
				switch (_v12) {
					case 1:
						return _Utils_update(
							model,
							{
								R: A2($author$project$PolygonCreator$movePoint, model.R, direct),
								c: $elm$core$Array$toList(
									A3(
										$elm$core$Array$set,
										model.j - 1,
										A2($author$project$PolygonCreator$movePoint, model.R, direct),
										$elm$core$Array$fromList(model.c)))
							});
					case 2:
						return _Utils_update(
							model,
							{
								af: A2($author$project$PolygonCreator$movePoint, model.af, direct),
								c: $elm$core$Array$toList(
									A3(
										$elm$core$Array$set,
										model.j - 1,
										A2($author$project$PolygonCreator$movePoint, model.af, direct),
										$elm$core$Array$fromList(model.c)))
							});
					case 3:
						return _Utils_update(
							model,
							{
								ag: A2($author$project$PolygonCreator$movePoint, model.ag, direct),
								c: $elm$core$Array$toList(
									A3(
										$elm$core$Array$set,
										model.j - 1,
										A2($author$project$PolygonCreator$movePoint, model.ag, direct),
										$elm$core$Array$fromList(model.c)))
							});
					case 4:
						return _Utils_update(
							model,
							{
								ah: A2($author$project$PolygonCreator$movePoint, model.ah, direct),
								c: $elm$core$Array$toList(
									A3(
										$elm$core$Array$set,
										model.j - 1,
										A2($author$project$PolygonCreator$movePoint, model.ah, direct),
										$elm$core$Array$fromList(model.c)))
							});
					case 5:
						return _Utils_update(
							model,
							{
								ai: A2($author$project$PolygonCreator$movePoint, model.ai, direct),
								c: $elm$core$Array$toList(
									A3(
										$elm$core$Array$set,
										model.j - 1,
										A2($author$project$PolygonCreator$movePoint, model.ai, direct),
										$elm$core$Array$fromList(model.c)))
							});
					case 6:
						return _Utils_update(
							model,
							{
								aj: A2($author$project$PolygonCreator$movePoint, model.aj, direct),
								c: $elm$core$Array$toList(
									A3(
										$elm$core$Array$set,
										model.j - 1,
										A2($author$project$PolygonCreator$movePoint, model.aj, direct),
										$elm$core$Array$fromList(model.c)))
							});
					case 7:
						return _Utils_update(
							model,
							{
								ak: A2($author$project$PolygonCreator$movePoint, model.ak, direct),
								c: $elm$core$Array$toList(
									A3(
										$elm$core$Array$set,
										model.j - 1,
										A2($author$project$PolygonCreator$movePoint, model.ak, direct),
										$elm$core$Array$fromList(model.c)))
							});
					case 8:
						return _Utils_update(
							model,
							{
								al: A2($author$project$PolygonCreator$movePoint, model.al, direct),
								c: $elm$core$Array$toList(
									A3(
										$elm$core$Array$set,
										model.j - 1,
										A2($author$project$PolygonCreator$movePoint, model.al, direct),
										$elm$core$Array$fromList(model.c)))
							});
					case 9:
						return _Utils_update(
							model,
							{
								am: A2($author$project$PolygonCreator$movePoint, model.am, direct),
								c: $elm$core$Array$toList(
									A3(
										$elm$core$Array$set,
										model.j - 1,
										A2($author$project$PolygonCreator$movePoint, model.am, direct),
										$elm$core$Array$fromList(model.c)))
							});
					case 10:
						return _Utils_update(
							model,
							{
								F: A2($author$project$PolygonCreator$movePoint, model.F, direct),
								c: $elm$core$Array$toList(
									A3(
										$elm$core$Array$set,
										model.j - 1,
										A2($author$project$PolygonCreator$movePoint, model.F, direct),
										$elm$core$Array$fromList(model.c)))
							});
					default:
						return _Utils_update(
							model,
							{R: model.R});
				}
			case 8:
				return _Utils_update(
					model,
					{
						bB: true,
						c: $author$project$PolygonCreator$setPoint(model)
					});
			case 9:
				return _Utils_update(
					model,
					{
						bA: true,
						bB: true,
						c: _List_fromArray(
							[
								_Utils_Tuple2(0, 0)
							])
					});
			case 10:
				var x = msg.a;
				return _Utils_update(
					model,
					{j: x, by: 0, bn: false});
			case 11:
				return _Utils_update(
					model,
					{
						j: -1,
						bn: true,
						z: (model.z < 8) ? (model.z + 1) : model.z,
						c: (model.z < 8) ? _Utils_ap(
							model.c,
							_List_fromArray(
								[
									_Utils_Tuple2(0, 0)
								])) : model.c,
						K: $elm$core$Array$toList(
							A3(
								$elm$core$Array$set,
								model.z + 1,
								true,
								$elm$core$Array$fromList(model.K)))
					});
			case 12:
				return _Utils_update(
					model,
					{
						j: -1,
						bn: true,
						z: (model.z > 0) ? (model.z - 1) : model.z,
						c: (model.z > 0) ? A2($elm$core$List$take, model.z, model.c) : model.c,
						K: $elm$core$Array$toList(
							A3(
								$elm$core$Array$set,
								model.z,
								false,
								$elm$core$Array$fromList(model.K)))
					});
			case 15:
				var dir = msg.a;
				switch (dir) {
					case 0:
						return _Utils_update(
							model,
							{A: 0});
					case 1:
						return _Utils_update(
							model,
							{A: 1});
					case 2:
						return _Utils_update(
							model,
							{A: 2});
					case 3:
						return _Utils_update(
							model,
							{A: 3});
					default:
						return _Utils_update(
							model,
							{A: 4});
				}
			case 13:
				var dir = msg.a;
				return _Utils_update(
					model,
					{M: dir});
			default:
				return _Utils_update(
					model,
					{M: 6});
		}
	});
var $author$project$ShapeCreator$Dashed = 2;
var $author$project$ShapeCreator$Dotdash = 4;
var $author$project$ShapeCreator$Dotted = 1;
var $author$project$ShapeCreator$Longdash = 3;
var $author$project$ShapeCreateAssets$accel = function (x) {
	return $elm$core$Basics$round(
		A3(
			$elm$core$Basics$clamp,
			0,
			12,
			A2($elm$core$Basics$pow, x, 2)) / 4);
};
var $author$project$ShapeCreator$cycleTxt = function (s) {
	switch (s) {
		case 'Hello':
			return 'Bonjour';
		case 'Bonjour':
			return 'नमस्ते (Namaste)';
		case 'नमस्ते (Namaste)':
			return 'Guten Tag';
		case 'Guten Tag':
			return 'ආයුබෝවන් (Āyubōwan)';
		case 'ආයුබෝවන් (Āyubōwan)':
			return 'Jó napot';
		case 'Jó napot':
			return 'Dobro utro';
		case 'Dobro utro':
			return 'ਸਤਿ ਸ੍ਰੀ ਅਕਾਲ (Sat Shri Akaal)';
		case 'ਸਤਿ ਸ੍ਰੀ ਅਕਾਲ (Sat Shri Akaal)':
			return 'ٱلسَّلَامُ عَلَيْكُمْ (Salam Alaykum)';
		default:
			return 'Hello';
	}
};
var $author$project$ShapeCreator$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var t = msg.a;
				return _Utils_update(
					model,
					{
						C: function () {
							var _v1 = model.M;
							switch (_v1) {
								case 2:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.C + $author$project$ShapeCreateAssets$accel(model.l));
								case 3:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.C - $author$project$ShapeCreateAssets$accel(model.l));
								default:
									return model.C;
							}
						}(),
						l: function () {
							var _v2 = model.M;
							if (_v2 === 6) {
								return 0;
							} else {
								return model.l + 0.1;
							}
						}(),
						D: function () {
							var _v3 = model.M;
							switch (_v3) {
								case 4:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.D + $author$project$ShapeCreateAssets$accel(model.l));
								case 5:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.D - $author$project$ShapeCreateAssets$accel(model.l));
								default:
									return model.D;
							}
						}(),
						G: function () {
							var _v4 = model.M;
							switch (_v4) {
								case 0:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.G + $author$project$ShapeCreateAssets$accel(model.l));
								case 1:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.G - $author$project$ShapeCreateAssets$accel(model.l));
								default:
									return model.G;
							}
						}(),
						aO: t
					});
			case 9:
				var dir = msg.a;
				return _Utils_update(
					model,
					{bj: 35, M: dir});
			case 1:
				var stencil = msg.a;
				return _Utils_update(
					model,
					{bT: stencil});
			case 2:
				var draw = msg.a;
				return _Utils_update(
					model,
					{aV: draw});
			case 3:
				return _Utils_update(
					model,
					{
						br: function () {
							var _v5 = model.br;
							switch (_v5) {
								case 0:
									return 1;
								case 1:
									return 2;
								case 2:
									return 3;
								case 3:
									return 4;
								default:
									return 0;
							}
						}()
					});
			case 4:
				return _Utils_update(
					model,
					{
						aP: $author$project$ShapeCreator$cycleTxt(model.aP)
					});
			case 6:
				switch (msg.a) {
					case 0:
						var _v6 = msg.a;
						return _Utils_update(
							model,
							{b5: !model.b5});
					case 1:
						var _v7 = msg.a;
						return _Utils_update(
							model,
							{a_: !model.a_});
					case 2:
						var _v8 = msg.a;
						return _Utils_update(
							model,
							{a$: !model.a$});
					case 3:
						var _v9 = msg.a;
						return _Utils_update(
							model,
							{a0: !model.a0});
					case 4:
						var _v10 = msg.a;
						return _Utils_update(
							model,
							{a1: !model.a1});
					default:
						var _v11 = msg.a;
						return _Utils_update(
							model,
							{aZ: !model.aZ});
				}
			case 7:
				var t = msg.a;
				return t(model);
			case 5:
				var clr = msg.a;
				return _Utils_update(
					model,
					{bj: clr});
			default:
				var notif = msg.a;
				return _Utils_update(
					model,
					{cc: notif});
		}
	});
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $author$project$SinCreator$curveX = function (x) {
	return $elm$core$Basics$round(
		A3(
			$elm$core$Basics$clamp,
			0,
			12,
			A2($elm$core$Basics$pow, x, 2)) / 4);
};
var $author$project$SinCreator$cycleFun = function (f) {
	switch (f) {
		case 0:
			return 1;
		case 1:
			return 2;
		default:
			return 0;
	}
};
var $author$project$SinCreator$EditableXSin = 8;
var $author$project$SinCreator$MakeTransparent = 7;
var $author$project$SinCreator$MoveCircle = 3;
var $author$project$SinCreator$MoveX = 1;
var $author$project$SinCreator$MoveY = 2;
var $author$project$SinCreator$ScaleX = 5;
var $author$project$SinCreator$ScaleY = 6;
var $author$project$SinCreator$URotate = 4;
var $author$project$SinCreator$cycleTransforms = function (tr) {
	switch (tr) {
		case 0:
			return 4;
		case 4:
			return 5;
		case 5:
			return 6;
		case 6:
			return 7;
		case 7:
			return 1;
		case 1:
			return 2;
		case 2:
			return 3;
		case 3:
			return 8;
		default:
			return 0;
	}
};
var $author$project$SinCreator$cycleTransformsReverse = function (tr) {
	switch (tr) {
		case 4:
			return 0;
		case 5:
			return 4;
		case 6:
			return 5;
		case 7:
			return 6;
		case 1:
			return 7;
		case 2:
			return 1;
		case 3:
			return 2;
		case 8:
			return 3;
		default:
			return 8;
	}
};
var $author$project$SinCreator$Cos = 1;
var $author$project$SinCreator$cycleTrig = function (f) {
	if (!f) {
		return 1;
	} else {
		return 0;
	}
};
var $author$project$SinCreator$eval = F3(
	function (f, u, v) {
		switch (f) {
			case 0:
				return u;
			case 1:
				return u;
			default:
				return v;
		}
	});
var $author$project$SinCreator$evalTrig = F2(
	function (f, u) {
		if (!f) {
			return $elm$core$Basics$sin(u);
		} else {
			return $elm$core$Basics$cos(u);
		}
	});
var $author$project$SinCreator$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var t = msg.a;
				var vArg = model.bX + (model.aH * (t - (t - 0.05)));
				var v = model.w * A2($author$project$SinCreator$evalTrig, model.bJ, vArg);
				var uArg = model.aE + (model.k * (t - (t - 0.05)));
				var uCosGraph = model.b * $elm$core$Basics$cos(uArg);
				var uSinGraph = model.b * $elm$core$Basics$sin(uArg);
				var u = model.b * A2($author$project$SinCreator$evalTrig, model.aD, uArg);
				var r = A3(
					$elm$core$Basics$clamp,
					0,
					255,
					$elm$core$Basics$abs(
						model.q * A3($author$project$SinCreator$eval, model.bb, u, v)));
				var g = A3(
					$elm$core$Basics$clamp,
					0,
					255,
					$elm$core$Basics$abs(
						model.p * A3($author$project$SinCreator$eval, model.aY, u, v)));
				var editableArg = model.b1 + (model.o * (t - (t - 0.05)));
				var editableYSinForTransforms = model.m * $elm$core$Basics$cos(editableArg);
				var currentTime = function () {
					var _v14 = model.aO;
					if (_v14.$ === 1) {
						return 0;
					} else {
						var ct = _v14.a;
						return ct;
					}
				}();
				var b = A3(
					$elm$core$Basics$clamp,
					0,
					255,
					$elm$core$Basics$abs(
						model.n * A3($author$project$SinCreator$eval, model.aS, u, v)));
				var cosGraphPoint = _Utils_Tuple3(
					uCosGraph,
					0,
					A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, r, g, b));
				var sinGraphPoint = _Utils_Tuple3(
					0,
					uSinGraph,
					A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, r, g, b));
				return _Utils_update(
					model,
					{
						aR: b,
						n: function () {
							var _v1 = model.M;
							switch (_v1) {
								case 12:
									return (model.n < 253) ? (model.n + $author$project$SinCreator$curveX(model.l)) : model.n;
								case 13:
									return (model.n > 2) ? (model.n - $author$project$SinCreator$curveX(model.l)) : model.n;
								default:
									return model.n;
							}
						}(),
						l: function () {
							var _v2 = model.M;
							if (_v2 === 16) {
								return 0;
							} else {
								return model.l + 0.1;
							}
						}(),
						bk: A2(
							$elm$core$List$take,
							2470,
							_Utils_ap(
								_List_fromArray(
									[cosGraphPoint]),
								A2(
									$elm$core$List$filterMap,
									function (_v3) {
										var xx = _v3.a;
										var yy = _v3.b;
										var cc = _v3.c;
										return (_Utils_cmp(yy, -model.b_) < 1) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
											_Utils_Tuple3(xx, yy - 0.35, cc));
									},
									model.bk))),
						b$: currentTime,
						o: function () {
							var _v4 = model.M;
							switch (_v4) {
								case 8:
									return (_Utils_cmp(model.o, model.x) < 0) ? (model.o + $author$project$SinCreator$curveX(model.l)) : ((_Utils_cmp(model.o, model.x) > 0) ? model.x : model.o);
								case 9:
									return (_Utils_cmp(model.o, -model.x) > 0) ? (model.o - $author$project$SinCreator$curveX(model.l)) : ((_Utils_cmp(model.o, -model.x) < 0) ? (-model.x) : model.o);
								default:
									return model.o;
							}
						}(),
						m: function () {
							var _v5 = model.M;
							switch (_v5) {
								case 6:
									return (_Utils_cmp(model.m, model.r) < 0) ? (model.m + $author$project$SinCreator$curveX(model.l)) : ((_Utils_cmp(model.m, model.r) > 0) ? model.r : model.m);
								case 7:
									return (_Utils_cmp(model.m, -model.r) > 0) ? (model.m - $author$project$SinCreator$curveX(model.l)) : ((_Utils_cmp(model.m, -model.r) < 0) ? (-model.r) : model.m);
								default:
									return model.m;
							}
						}(),
						bl: function () {
							var _v6 = model.M;
							switch (_v6) {
								case 4:
									return model.bl + $author$project$SinCreator$curveX(model.l);
								case 5:
									return model.bl - $author$project$SinCreator$curveX(model.l);
								default:
									return model.bl;
							}
						}(),
						aX: g,
						p: function () {
							var _v7 = model.M;
							switch (_v7) {
								case 14:
									return (model.p < 252) ? (model.p + $author$project$SinCreator$curveX(model.l)) : model.p;
								case 15:
									return (model.p > 2) ? (model.p - $author$project$SinCreator$curveX(model.l)) : model.p;
								default:
									return model.p;
							}
						}(),
						ba: r,
						q: function () {
							var _v8 = model.M;
							switch (_v8) {
								case 10:
									return (model.q < 253) ? (model.q + $author$project$SinCreator$curveX(model.l)) : model.q;
								case 11:
									return (model.q > 2) ? (model.q - $author$project$SinCreator$curveX(model.l)) : model.q;
								default:
									return model.q;
							}
						}(),
						bq: A2(
							$elm$core$List$take,
							2470,
							_Utils_ap(
								_List_fromArray(
									[sinGraphPoint]),
								A2(
									$elm$core$List$filterMap,
									function (_v9) {
										var xx = _v9.a;
										var yy = _v9.b;
										var cc = _v9.c;
										return (_Utils_cmp(xx, model.bE) > -1) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
											_Utils_Tuple3(xx + 0.35, yy, cc));
									},
									model.bq))),
						aO: $elm$core$Maybe$Just(t),
						bK: u,
						aE: uArg,
						be: uCosGraph,
						k: function () {
							var _v10 = model.M;
							switch (_v10) {
								case 2:
									return (_Utils_cmp(model.k, model.x) < 0) ? (model.k + $author$project$SinCreator$curveX(model.l)) : ((_Utils_cmp(model.k, model.x) > 0) ? model.x : model.k);
								case 3:
									return (_Utils_cmp(model.k, -model.x) > 0) ? (model.k - $author$project$SinCreator$curveX(model.l)) : ((_Utils_cmp(model.k, -model.x) < 0) ? (-model.x) : model.k);
								default:
									return model.k;
							}
						}(),
						b: function () {
							var _v11 = model.M;
							switch (_v11) {
								case 0:
									return (_Utils_cmp(model.b, model.r) < 0) ? (model.b + $author$project$SinCreator$curveX(model.l)) : ((_Utils_cmp(model.b, model.r) > 0) ? model.r : model.b);
								case 1:
									return (_Utils_cmp(model.b, -model.r) > 0) ? (model.b - $author$project$SinCreator$curveX(model.l)) : ((_Utils_cmp(model.b, -model.r) < 0) ? (-model.r) : model.b);
								default:
									return model.b;
							}
						}(),
						L: function () {
							var _v12 = model.M;
							switch (_v12) {
								case 4:
									return model.L + $author$project$SinCreator$curveX(model.l);
								case 5:
									return model.L - $author$project$SinCreator$curveX(model.l);
								default:
									return model.L;
							}
						}(),
						aF: uSinGraph,
						bW: v,
						bX: vArg,
						w: function () {
							var _v13 = model.M;
							switch (_v13) {
								case 17:
									return (model.w < 48) ? (model.w + $author$project$SinCreator$curveX(model.l)) : model.w;
								case 18:
									return (_Utils_cmp(model.w, -48) > 0) ? (model.w - $author$project$SinCreator$curveX(model.l)) : model.w;
								default:
									return model.w;
							}
						}()
					});
			case 1:
				var t = msg.a;
				return t(model);
			case 2:
				var notif = msg.a;
				return _Utils_update(
					model,
					{cc: notif});
			case 3:
				return _Utils_update(
					model,
					{
						bb: $author$project$SinCreator$cycleFun(model.bb)
					});
			case 4:
				return _Utils_update(
					model,
					{
						aY: $author$project$SinCreator$cycleFun(model.aY)
					});
			case 5:
				return _Utils_update(
					model,
					{
						aS: $author$project$SinCreator$cycleFun(model.aS)
					});
			case 24:
				return _Utils_update(
					model,
					{
						q: (model.q < 255) ? (model.q + 1) : model.q
					});
			case 25:
				return _Utils_update(
					model,
					{
						q: (model.q > 0) ? (model.q - 1) : model.q
					});
			case 26:
				return _Utils_update(
					model,
					{
						p: (model.p < 255) ? (model.p + 1) : model.p
					});
			case 27:
				return _Utils_update(
					model,
					{
						p: (model.p > 0) ? (model.p - 1) : model.p
					});
			case 28:
				return _Utils_update(
					model,
					{
						n: (model.n < 255) ? (model.n + 1) : model.n
					});
			case 29:
				return _Utils_update(
					model,
					{
						n: (model.n > 0) ? (model.n - 1) : model.n
					});
			case 6:
				return _Utils_update(
					model,
					{
						b: (_Utils_cmp(model.b, model.r) < 0) ? (model.b + 1) : model.b
					});
			case 9:
				return _Utils_update(
					model,
					{
						b: (_Utils_cmp(model.b, -model.r) > 0) ? (model.b - 1) : model.b
					});
			case 7:
				return _Utils_update(
					model,
					{
						k: (_Utils_cmp(model.k, model.x) < 0) ? (model.k + 1) : model.k
					});
			case 10:
				return _Utils_update(
					model,
					{
						k: (model.k > 0) ? (model.k - 1) : model.k
					});
			case 8:
				return _Utils_update(
					model,
					{aE: model.aE + ((model.bv * $elm$core$Basics$pi) / 4), L: model.L + model.bv});
			case 11:
				return _Utils_update(
					model,
					{aE: model.aE - ((model.bv * $elm$core$Basics$pi) / 4), L: model.L - model.bv});
			case 12:
				return _Utils_update(
					model,
					{
						m: (_Utils_cmp(model.m, model.r) < 0) ? (model.m + 1) : model.m
					});
			case 14:
				return _Utils_update(
					model,
					{
						m: (_Utils_cmp(model.m, -model.r) > 0) ? (model.m - 1) : model.m
					});
			case 13:
				return _Utils_update(
					model,
					{
						o: (_Utils_cmp(model.o, model.x) < 0) ? (model.o + 1) : model.o
					});
			case 15:
				return _Utils_update(
					model,
					{
						o: (_Utils_cmp(model.o, -model.x) > 0) ? (model.o - 1) : model.o
					});
			case 16:
				return _Utils_update(
					model,
					{
						w: (_Utils_cmp(model.w, model.r) < 0) ? (model.w + 1) : model.w
					});
			case 17:
				return _Utils_update(
					model,
					{
						w: (_Utils_cmp(model.w, -model.r) > 0) ? (model.w - 1) : model.w
					});
			case 18:
				return _Utils_update(
					model,
					{
						aH: (_Utils_cmp(model.aH, model.x) < 0) ? (model.aH + 1) : model.aH
					});
			case 19:
				return _Utils_update(
					model,
					{aH: model.aH - 1});
			case 20:
				return _Utils_update(
					model,
					{
						aD: $author$project$SinCreator$cycleTrig(model.aD)
					});
			case 21:
				return _Utils_update(
					model,
					{
						bJ: $author$project$SinCreator$cycleTrig(model.bJ)
					});
			case 22:
				return _Utils_update(
					model,
					{
						ac: $author$project$SinCreator$cycleTransforms(model.ac)
					});
			case 23:
				return _Utils_update(
					model,
					{
						ac: $author$project$SinCreator$cycleTransformsReverse(model.ac)
					});
			case 30:
				var dir = msg.a;
				return _Utils_update(
					model,
					{M: dir});
			default:
				return _Utils_update(
					model,
					{M: 16});
		}
	});
var $author$project$TextCreator$Dashed = 2;
var $author$project$TextCreator$Dotdash = 4;
var $author$project$TextCreator$Dotted = 1;
var $author$project$TextCreator$Longdash = 3;
var $elm$core$String$dropRight = F2(
	function (n, string) {
		return (n < 1) ? string : A3($elm$core$String$slice, 0, -n, string);
	});
var $author$project$TextCreator$setTxt = F2(
	function (m, s) {
		return _Utils_ap(m.aP, s);
	});
var $author$project$TextCreator$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var t = msg.a;
				return ((model.bN > 2) ? function (m) {
					return _Utils_update(
						m,
						{
							aP: A2($elm$core$String$dropRight, 1, model.aP)
						});
				} : function (m) {
					return m;
				})(
					_Utils_update(
						model,
						{
							C: function () {
								var _v1 = model.M;
								switch (_v1) {
									case 2:
										return A3(
											$elm$core$Basics$clamp,
											0,
											255,
											model.C + $author$project$ShapeCreateAssets$accel(model.l));
									case 3:
										return A3(
											$elm$core$Basics$clamp,
											0,
											255,
											model.C - $author$project$ShapeCreateAssets$accel(model.l));
									default:
										return model.C;
								}
							}(),
							l: function () {
								var _v2 = model.M;
								if (_v2 === 7) {
									return 0;
								} else {
									return model.l + 0.1;
								}
							}(),
							bN: function () {
								var _v3 = model.M;
								if (_v3 === 6) {
									return $author$project$ShapeCreateAssets$accel(model.l);
								} else {
									return 0;
								}
							}(),
							D: function () {
								var _v4 = model.M;
								switch (_v4) {
									case 4:
										return A3(
											$elm$core$Basics$clamp,
											0,
											255,
											model.D + $author$project$ShapeCreateAssets$accel(model.l));
									case 5:
										return A3(
											$elm$core$Basics$clamp,
											0,
											255,
											model.D - $author$project$ShapeCreateAssets$accel(model.l));
									default:
										return model.D;
								}
							}(),
							bP: function () {
								var _v5 = model.M;
								if (_v5 === 6) {
									return model.bP;
								} else {
									return 0;
								}
							}(),
							G: function () {
								var _v6 = model.M;
								switch (_v6) {
									case 0:
										return A3(
											$elm$core$Basics$clamp,
											0,
											255,
											model.G + $author$project$ShapeCreateAssets$accel(model.l));
									case 1:
										return A3(
											$elm$core$Basics$clamp,
											0,
											255,
											model.G - $author$project$ShapeCreateAssets$accel(model.l));
									default:
										return model.G;
								}
							}(),
							aO: t
						}));
			case 12:
				return _Utils_update(
					model,
					{M: 7});
			case 11:
				var dir = msg.a;
				return _Utils_update(
					model,
					{M: dir});
			case 1:
				var stencil = msg.a;
				return _Utils_update(
					model,
					{bT: stencil});
			case 2:
				var draw = msg.a;
				return _Utils_update(
					model,
					{aV: draw});
			case 3:
				return _Utils_update(
					model,
					{
						br: function () {
							var _v7 = model.br;
							switch (_v7) {
								case 0:
									return 1;
								case 1:
									return 2;
								case 2:
									return 3;
								case 3:
									return 4;
								default:
									return 1;
							}
						}()
					});
			case 5:
				switch (msg.a) {
					case 0:
						var _v8 = msg.a;
						return _Utils_update(
							model,
							{b5: !model.b5});
					case 1:
						var _v9 = msg.a;
						return _Utils_update(
							model,
							{a_: !model.a_});
					case 2:
						var _v10 = msg.a;
						return _Utils_update(
							model,
							{a$: !model.a$});
					case 3:
						var _v11 = msg.a;
						return _Utils_update(
							model,
							{a0: !model.a0});
					case 4:
						var _v12 = msg.a;
						return _Utils_update(
							model,
							{a1: !model.a1});
					case 5:
						var _v13 = msg.a;
						return _Utils_update(
							model,
							{aZ: !model.aZ});
					case 6:
						var _v14 = msg.a;
						return _Utils_update(
							model,
							{ae: !model.ae});
					case 14:
						var _v15 = msg.a;
						return _Utils_update(
							model,
							{aw: !model.aw});
					case 7:
						var _v16 = msg.a;
						return _Utils_update(
							model,
							{aq: !model.aq});
					case 8:
						var _v17 = msg.a;
						return _Utils_update(
							model,
							{at: !model.at});
					case 9:
						var _v18 = msg.a;
						return _Utils_update(
							model,
							{ay: !model.ay});
					case 10:
						var _v19 = msg.a;
						return _Utils_update(
							model,
							{ax: !model.ax});
					case 11:
						var _v20 = msg.a;
						return _Utils_update(
							model,
							{au: !model.au});
					case 12:
						var _v21 = msg.a;
						return _Utils_update(
							model,
							{av: !model.av});
					case 13:
						var _v22 = msg.a;
						return _Utils_update(
							model,
							{as: !model.as});
					case 15:
						var _v23 = msg.a;
						return _Utils_update(
							model,
							{ar: !model.ar});
					default:
						var _v24 = msg.a;
						return model;
				}
			case 6:
				var t = msg.a;
				return t(model);
			case 4:
				var clr = msg.a;
				return _Utils_update(
					model,
					{bj: clr});
			case 7:
				var notif = msg.a;
				return _Utils_update(
					model,
					{cc: notif});
			case 8:
				var s = msg.a;
				return _Utils_update(
					model,
					{
						aP: A2($author$project$TextCreator$setTxt, model, s)
					});
			case 10:
				return _Utils_update(
					model,
					{
						aL: function () {
							var _v25 = model.aL;
							switch (_v25) {
								case 1:
									return 2;
								case 2:
									return 3;
								case 3:
									return 4;
								default:
									return 1;
							}
						}()
					});
			default:
				return _Utils_update(
					model,
					{
						aP: A2($elm$core$String$dropRight, 1, model.aP)
					});
		}
	});
var $author$project$TriCreator$Dashed = 2;
var $author$project$TriCreator$Dotdash = 4;
var $author$project$TriCreator$Dotted = 1;
var $author$project$TriCreator$Longdash = 3;
var $author$project$ShapeCreateAssets$angleBound = function (angle) {
	return ((_Utils_cmp(
		$elm$core$Basics$degrees(angle),
		2 * $elm$core$Basics$pi) < 0) && ($elm$core$Basics$degrees(angle) >= 0)) ? $elm$core$Basics$round(angle) : (($elm$core$Basics$degrees(angle) < 0) ? (360 - A2(
		$elm$core$Basics$modBy,
		360,
		(-1) * $elm$core$Basics$round(angle))) : A2(
		$elm$core$Basics$modBy,
		360,
		$elm$core$Basics$round(angle)));
};
var $author$project$TriCreator$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var t = msg.a;
				return _Utils_update(
					model,
					{
						ad: function () {
							var _v1 = model.M;
							switch (_v1) {
								case 33:
									return A3(
										$elm$core$Basics$clamp,
										0,
										1,
										model.ad + (0.25 * $author$project$ShapeCreateAssets$accel(model.l)));
								case 32:
									return A3(
										$elm$core$Basics$clamp,
										0,
										1,
										model.ad - (0.25 * $author$project$ShapeCreateAssets$accel(model.l)));
								default:
									return model.ad;
							}
						}(),
						aI: function () {
							var _v2 = model.M;
							switch (_v2) {
								case 22:
									return $author$project$ShapeCreateAssets$angleBound(
										model.aI - $author$project$ShapeCreateAssets$accel(model.l));
								case 23:
									return $author$project$ShapeCreateAssets$angleBound(
										model.aI + $author$project$ShapeCreateAssets$accel(model.l));
								default:
									return $author$project$ShapeCreateAssets$angleBound(model.aI);
							}
						}(),
						S: function () {
							var _v3 = model.M;
							switch (_v3) {
								case 34:
									return A3(
										$elm$core$Basics$clamp,
										$elm$core$Basics$degrees(-180),
										$elm$core$Basics$degrees(180),
										model.S + $elm$core$Basics$degrees(
											$author$project$ShapeCreateAssets$accel(model.l)));
								case 35:
									return A3(
										$elm$core$Basics$clamp,
										$elm$core$Basics$degrees(-180),
										$elm$core$Basics$degrees(180),
										model.S - $elm$core$Basics$degrees(
											$author$project$ShapeCreateAssets$accel(model.l)));
								default:
									return model.S;
							}
						}(),
						C: function () {
							var _v4 = model.M;
							switch (_v4) {
								case 18:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.C + $author$project$ShapeCreateAssets$accel(model.l));
								case 19:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.C - $author$project$ShapeCreateAssets$accel(model.l));
								default:
									return model.C;
							}
						}(),
						l: function () {
							var _v5 = model.M;
							if (_v5 === 36) {
								return 0;
							} else {
								return model.l + 0.1;
							}
						}(),
						D: function () {
							var _v6 = model.M;
							switch (_v6) {
								case 20:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.D + $author$project$ShapeCreateAssets$accel(model.l));
								case 21:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.D - $author$project$ShapeCreateAssets$accel(model.l));
								default:
									return model.D;
							}
						}(),
						cS: function () {
							var _v7 = model.M;
							switch (_v7) {
								case 28:
									return A3(
										$elm$core$Basics$clamp,
										10,
										100,
										model.cS + $author$project$ShapeCreateAssets$accel(model.l));
								case 29:
									return A3(
										$elm$core$Basics$clamp,
										10,
										100,
										model.cS - $author$project$ShapeCreateAssets$accel(model.l));
								default:
									return model.cS;
							}
						}(),
						U: function () {
							var _v8 = model.M;
							switch (_v8) {
								case 6:
									return model.U - $author$project$ShapeCreateAssets$accel(model.l);
								case 7:
									return model.U + $author$project$ShapeCreateAssets$accel(model.l);
								default:
									return model.U;
							}
						}(),
						V: function () {
							var _v9 = model.M;
							switch (_v9) {
								case 5:
									return model.V - $author$project$ShapeCreateAssets$accel(model.l);
								case 4:
									return model.V + $author$project$ShapeCreateAssets$accel(model.l);
								default:
									return model.V;
							}
						}(),
						W: function () {
							var _v10 = model.M;
							switch (_v10) {
								case 14:
									return model.W - $author$project$ShapeCreateAssets$accel(model.l);
								case 15:
									return model.W + $author$project$ShapeCreateAssets$accel(model.l);
								default:
									return model.W;
							}
						}(),
						X: function () {
							var _v11 = model.M;
							switch (_v11) {
								case 13:
									return model.X - $author$project$ShapeCreateAssets$accel(model.l);
								case 12:
									return model.X + $author$project$ShapeCreateAssets$accel(model.l);
								default:
									return model.X;
							}
						}(),
						Y: function () {
							var _v12 = model.M;
							switch (_v12) {
								case 10:
									return model.Y - $author$project$ShapeCreateAssets$accel(model.l);
								case 11:
									return model.Y + $author$project$ShapeCreateAssets$accel(model.l);
								default:
									return model.Y;
							}
						}(),
						Z: function () {
							var _v13 = model.M;
							switch (_v13) {
								case 9:
									return model.Z - $author$project$ShapeCreateAssets$accel(model.l);
								case 8:
									return model.Z + $author$project$ShapeCreateAssets$accel(model.l);
								default:
									return model.Z;
							}
						}(),
						G: function () {
							var _v14 = model.M;
							switch (_v14) {
								case 16:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.G + $author$project$ShapeCreateAssets$accel(model.l));
								case 17:
									return A3(
										$elm$core$Basics$clamp,
										0,
										255,
										model.G - $author$project$ShapeCreateAssets$accel(model.l));
								default:
									return model.G;
							}
						}(),
						aO: t,
						dl: function () {
							var _v15 = model.M;
							switch (_v15) {
								case 26:
									return A3(
										$elm$core$Basics$clamp,
										10,
										100,
										model.dl + $author$project$ShapeCreateAssets$accel(model.l));
								case 27:
									return A3(
										$elm$core$Basics$clamp,
										10,
										100,
										model.dl - $author$project$ShapeCreateAssets$accel(model.l));
								default:
									return model.dl;
							}
						}(),
						cv: function () {
							var _v16 = model.M;
							switch (_v16) {
								case 2:
									return model.cv - $author$project$ShapeCreateAssets$accel(model.l);
								case 3:
									return model.cv + $author$project$ShapeCreateAssets$accel(model.l);
								default:
									return model.cv;
							}
						}(),
						bg: function () {
							var _v17 = model.M;
							switch (_v17) {
								case 0:
									return model.bg + $author$project$ShapeCreateAssets$accel(model.l);
								case 1:
									return model.bg - $author$project$ShapeCreateAssets$accel(model.l);
								default:
									return model.bg;
							}
						}()
					});
			case 1:
				var stencil = msg.a;
				return _Utils_update(
					model,
					{bT: stencil});
			case 2:
				var draw = msg.a;
				return _Utils_update(
					model,
					{aV: draw});
			case 3:
				return _Utils_update(
					model,
					{
						br: function () {
							var _v18 = model.br;
							switch (_v18) {
								case 0:
									return 1;
								case 1:
									return 2;
								case 2:
									return 3;
								case 3:
									return 4;
								default:
									return 0;
							}
						}()
					});
			case 9:
				return _Utils_update(
					model,
					{M: 36});
			case 8:
				var dir = msg.a;
				return _Utils_update(
					model,
					{M: dir});
			case 5:
				switch (msg.a) {
					case 0:
						var _v19 = msg.a;
						return _Utils_update(
							model,
							{b5: !model.b5});
					case 1:
						var _v20 = msg.a;
						return _Utils_update(
							model,
							{a_: !model.a_});
					case 2:
						var _v21 = msg.a;
						return _Utils_update(
							model,
							{a$: !model.a$});
					case 3:
						var _v22 = msg.a;
						return _Utils_update(
							model,
							{a0: !model.a0});
					case 4:
						var _v23 = msg.a;
						return _Utils_update(
							model,
							{a1: !model.a1});
					default:
						var _v24 = msg.a;
						return _Utils_update(
							model,
							{aZ: !model.aZ});
				}
			case 6:
				var t = msg.a;
				return t(model);
			case 4:
				var clr = msg.a;
				return _Utils_update(
					model,
					{bj: clr});
			default:
				var notif = msg.a;
				return _Utils_update(
					model,
					{cc: notif});
		}
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 13:
				return _Utils_update(
					model,
					{g: 1});
			case 14:
				return _Utils_update(
					model,
					{i: 1});
			case 15:
				return _Utils_update(
					model,
					{h: 1});
			case 16:
				return _Utils_update(
					model,
					{f: 1});
			case 17:
				return _Utils_update(
					model,
					{e: 1});
			case 18:
				return _Utils_update(
					model,
					{d: 1});
			case 19:
				return _Utils_update(
					model,
					{
						g: function () {
							var _v1 = model.a;
							if (!_v1) {
								return model.g;
							} else {
								return 0;
							}
						}()
					});
			case 20:
				return _Utils_update(
					model,
					{
						i: function () {
							var _v2 = model.a;
							if (_v2 === 1) {
								return model.i;
							} else {
								return 0;
							}
						}()
					});
			case 21:
				return _Utils_update(
					model,
					{
						h: function () {
							var _v3 = model.a;
							if (_v3 === 2) {
								return model.h;
							} else {
								return 0;
							}
						}()
					});
			case 22:
				return _Utils_update(
					model,
					{
						f: function () {
							var _v4 = model.a;
							if (_v4 === 3) {
								return model.f;
							} else {
								return 0;
							}
						}()
					});
			case 23:
				return _Utils_update(
					model,
					{
						e: function () {
							var _v5 = model.a;
							if (_v5 === 4) {
								return model.e;
							} else {
								return 0;
							}
						}()
					});
			case 24:
				return _Utils_update(
					model,
					{
						d: function () {
							var _v6 = model.a;
							if (_v6 === 5) {
								return model.d;
							} else {
								return 0;
							}
						}()
					});
			default:
				var _v7 = model.a;
				switch (_v7) {
					case 0:
						switch (msg.$) {
							case 0:
								var f = msg.a;
								var g = msg.b;
								return _Utils_update(
									model,
									{
										a4: A2(
											$author$project$ShapeCreator$update,
											A2($author$project$ShapeCreator$Tick, f, g),
											model.a4)
									});
							case 1:
								var m1 = msg.a;
								return _Utils_update(
									model,
									{
										a4: A2($author$project$ShapeCreator$update, m1, model.a4)
									});
							case 2:
								return model;
							case 3:
								return model;
							case 4:
								return model;
							case 5:
								return model;
							case 6:
								return model;
							case 7:
								return _Utils_update(
									model,
									{e: 0, f: 0, a: 0, d: 0, h: 0, i: 0});
							case 8:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 1, d: 0, h: 0});
							case 9:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 2, d: 0, i: 0});
							case 10:
								return _Utils_update(
									model,
									{e: 0, g: 0, a: 3, d: 0, h: 0, i: 0});
							case 11:
								return _Utils_update(
									model,
									{f: 0, g: 0, a: 4, d: 0, h: 0, i: 0});
							case 12:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 5, h: 0, i: 0});
							default:
								return model;
						}
					case 1:
						switch (msg.$) {
							case 0:
								var f = msg.a;
								var g = msg.b;
								return _Utils_update(
									model,
									{
										a5: A2(
											$author$project$TriCreator$update,
											A2($author$project$TriCreator$Tick, f, g),
											model.a5)
									});
							case 1:
								return model;
							case 2:
								var m2 = msg.a;
								return _Utils_update(
									model,
									{
										a5: A2($author$project$TriCreator$update, m2, model.a5)
									});
							case 3:
								return model;
							case 4:
								return model;
							case 5:
								return model;
							case 6:
								return model;
							case 7:
								return _Utils_update(
									model,
									{e: 0, f: 0, a: 0, d: 0, h: 0, i: 0});
							case 8:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 1, d: 0, h: 0});
							case 9:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 2, d: 0, i: 0});
							case 10:
								return _Utils_update(
									model,
									{e: 0, g: 0, a: 3, d: 0, h: 0, i: 0});
							case 11:
								return _Utils_update(
									model,
									{f: 0, g: 0, a: 4, d: 0, h: 0, i: 0});
							case 12:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 5, h: 0, i: 0});
							default:
								return model;
						}
					case 2:
						switch (msg.$) {
							case 0:
								var f = msg.a;
								var g = msg.b;
								return _Utils_update(
									model,
									{
										a6: A2(
											$author$project$PolygonCreator$update,
											A2($author$project$PolygonCreator$Tick, f, g),
											model.a6)
									});
							case 1:
								return model;
							case 2:
								return model;
							case 3:
								var m3 = msg.a;
								return _Utils_update(
									model,
									{
										a6: A2($author$project$PolygonCreator$update, m3, model.a6)
									});
							case 4:
								return model;
							case 5:
								return model;
							case 6:
								return model;
							case 7:
								return _Utils_update(
									model,
									{e: 0, f: 0, a: 0, d: 0, h: 0, i: 0});
							case 8:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 1, d: 0, h: 0});
							case 9:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 2, d: 0, i: 0});
							case 10:
								return _Utils_update(
									model,
									{e: 0, g: 0, a: 3, d: 0, h: 0, i: 0});
							case 11:
								return _Utils_update(
									model,
									{f: 0, g: 0, a: 4, d: 0, h: 0, i: 0});
							case 12:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 5, d: 0, h: 0, i: 0});
							default:
								return model;
						}
					case 3:
						switch (msg.$) {
							case 0:
								var f = msg.a;
								var g = msg.b;
								return _Utils_update(
									model,
									{
										a7: A2(
											$author$project$ArcCreator$update,
											A2($author$project$ArcCreator$Tick, f, g),
											model.a7)
									});
							case 1:
								return model;
							case 2:
								return model;
							case 3:
								return model;
							case 4:
								var m4 = msg.a;
								return _Utils_update(
									model,
									{
										a7: A2($author$project$ArcCreator$update, m4, model.a7)
									});
							case 5:
								return model;
							case 6:
								return model;
							case 7:
								return _Utils_update(
									model,
									{e: 0, f: 0, a: 0, d: 0, h: 0, i: 0});
							case 8:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 1, d: 0, h: 0});
							case 9:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 2, d: 0, i: 0});
							case 10:
								return _Utils_update(
									model,
									{e: 0, g: 0, a: 3, d: 0, h: 0, i: 0});
							case 11:
								return _Utils_update(
									model,
									{f: 0, g: 0, a: 4, d: 0, h: 0, i: 0});
							case 12:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 5, h: 0, i: 0});
							default:
								return model;
						}
					case 4:
						switch (msg.$) {
							case 0:
								var f = msg.a;
								var g = msg.b;
								return _Utils_update(
									model,
									{
										a8: A2(
											$author$project$SinCreator$update,
											A2($author$project$SinCreator$Tick, f, g),
											model.a8)
									});
							case 1:
								return model;
							case 2:
								return model;
							case 3:
								return model;
							case 4:
								return model;
							case 5:
								var m5 = msg.a;
								return _Utils_update(
									model,
									{
										a8: A2($author$project$SinCreator$update, m5, model.a8)
									});
							case 6:
								return model;
							case 7:
								return _Utils_update(
									model,
									{e: 0, f: 0, a: 0, d: 0, h: 0, i: 0});
							case 8:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 1, d: 0, h: 0});
							case 9:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 2, d: 0, i: 0});
							case 10:
								return _Utils_update(
									model,
									{e: 0, g: 0, a: 3, d: 0, h: 0, i: 0});
							case 11:
								return _Utils_update(
									model,
									{f: 0, g: 0, a: 4, d: 0, h: 0, i: 0});
							case 12:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 5, h: 0, i: 0});
							default:
								return model;
						}
					default:
						switch (msg.$) {
							case 0:
								var f = msg.a;
								var g = msg.b;
								return _Utils_update(
									model,
									{
										a9: A2(
											$author$project$TextCreator$update,
											A2($author$project$TextCreator$Tick, f, g),
											model.a9)
									});
							case 1:
								return model;
							case 2:
								return model;
							case 3:
								return model;
							case 4:
								return model;
							case 5:
								return model;
							case 6:
								var m6 = msg.a;
								return _Utils_update(
									model,
									{
										a9: A2($author$project$TextCreator$update, m6, model.a9)
									});
							case 7:
								return _Utils_update(
									model,
									{e: 0, f: 0, a: 0, d: 0, h: 0, i: 0});
							case 8:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 1, d: 0, h: 0});
							case 9:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 2, d: 0, i: 0});
							case 10:
								return _Utils_update(
									model,
									{e: 0, g: 0, a: 3, d: 0, h: 0, i: 0});
							case 11:
								return _Utils_update(
									model,
									{f: 0, g: 0, a: 4, d: 0, h: 0, i: 0});
							case 12:
								return _Utils_update(
									model,
									{e: 0, f: 0, g: 0, a: 5, h: 0, i: 0});
							default:
								return model;
						}
				}
		}
	});
var $author$project$Main$Goto1 = {$: 7};
var $author$project$Main$Goto2 = {$: 8};
var $author$project$Main$Goto3 = {$: 9};
var $author$project$Main$Goto4 = {$: 10};
var $author$project$Main$Goto5 = {$: 11};
var $author$project$Main$Goto6 = {$: 12};
var $author$project$Main$In1 = {$: 13};
var $author$project$Main$In2 = {$: 14};
var $author$project$Main$In3 = {$: 15};
var $author$project$Main$In4 = {$: 16};
var $author$project$Main$In5 = {$: 17};
var $author$project$Main$In6 = {$: 18};
var $author$project$Main$MoveInRect = function (a) {
	return {$: 25, a: a};
};
var $author$project$Main$Msg1 = function (a) {
	return {$: 1, a: a};
};
var $author$project$Main$Msg2 = function (a) {
	return {$: 2, a: a};
};
var $author$project$Main$Msg3 = function (a) {
	return {$: 3, a: a};
};
var $author$project$Main$Msg4 = function (a) {
	return {$: 4, a: a};
};
var $author$project$Main$Msg5 = function (a) {
	return {$: 5, a: a};
};
var $author$project$Main$Msg6 = function (a) {
	return {$: 6, a: a};
};
var $author$project$Main$Out1 = {$: 19};
var $author$project$Main$Out2 = {$: 20};
var $author$project$Main$Out3 = {$: 21};
var $author$project$Main$Out4 = {$: 22};
var $author$project$Main$Out5 = {$: 23};
var $author$project$Main$Out6 = {$: 24};
var $MacCASOutreach$graphicsvg$GraphicSVG$Pull = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$NoLine = {$: 0};
var $MacCASOutreach$graphicsvg$GraphicSVG$blank = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 0, 0, 0, 0);
var $MacCASOutreach$graphicsvg$GraphicSVG$subtract = F2(
	function (shape1, shape2) {
		return A2($MacCASOutreach$graphicsvg$GraphicSVG$AlphaMask, shape1, shape2);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$addOutline = F3(
	function (style, outlineClr, shape) {
		addOutline:
		while (true) {
			var lineStyle = function () {
				if (!style.$) {
					return $elm$core$Maybe$Nothing;
				} else {
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(style, outlineClr));
				}
			}();
			switch (shape.$) {
				case 0:
					var clr = shape.a;
					var st = shape.c;
					return A3($MacCASOutreach$graphicsvg$GraphicSVG$Inked, clr, lineStyle, st);
				case 2:
					var s = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$Move,
						s,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 3:
					var r = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$Rotate,
						r,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 4:
					var sx = shape.a;
					var sy = shape.b;
					var sh = shape.c;
					return A3(
						$MacCASOutreach$graphicsvg$GraphicSVG$Scale,
						sx,
						sy,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 5:
					var skx = shape.a;
					var sky = shape.b;
					var sh = shape.c;
					return A3(
						$MacCASOutreach$graphicsvg$GraphicSVG$Skew,
						skx,
						sky,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 6:
					var tm = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$Transformed,
						tm,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 7:
					var list = shape.a;
					var innerlist = A2(
						$elm$core$List$filterMap,
						function (shp) {
							if (shp.$ === 8) {
								return $elm$core$Maybe$Nothing;
							} else {
								return $elm$core$Maybe$Just(
									A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, $MacCASOutreach$graphicsvg$GraphicSVG$NoLine, $MacCASOutreach$graphicsvg$GraphicSVG$black, shp));
							}
						},
						list);
					if (!innerlist.b) {
						return $MacCASOutreach$graphicsvg$GraphicSVG$Group(_List_Nil);
					} else {
						if (!innerlist.b.b) {
							var hd = innerlist.a;
							var $temp$style = style,
								$temp$outlineClr = outlineClr,
								$temp$shape = hd;
							style = $temp$style;
							outlineClr = $temp$outlineClr;
							shape = $temp$shape;
							continue addOutline;
						} else {
							if (_Utils_eq(lineStyle, $elm$core$Maybe$Nothing)) {
								return $MacCASOutreach$graphicsvg$GraphicSVG$Group(innerlist);
							} else {
								var outlnshp = $MacCASOutreach$graphicsvg$GraphicSVG$GroupOutline(
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$subtract,
										$MacCASOutreach$graphicsvg$GraphicSVG$Group(innerlist),
										$MacCASOutreach$graphicsvg$GraphicSVG$Group(
											A2(
												$elm$core$List$map,
												A2($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr),
												innerlist))));
								return $MacCASOutreach$graphicsvg$GraphicSVG$Group(
									_Utils_ap(
										innerlist,
										_List_fromArray(
											[outlnshp])));
							}
						}
					}
				case 8:
					var cmbndshp = shape.a;
					return $MacCASOutreach$graphicsvg$GraphicSVG$GroupOutline(cmbndshp);
				case 9:
					var reg = shape.a;
					var sh = shape.b;
					var ptrn = A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, $MacCASOutreach$graphicsvg$GraphicSVG$NoLine, $MacCASOutreach$graphicsvg$GraphicSVG$black, reg);
					var inside = A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, $MacCASOutreach$graphicsvg$GraphicSVG$NoLine, $MacCASOutreach$graphicsvg$GraphicSVG$black, sh);
					if (_Utils_eq(lineStyle, $elm$core$Maybe$Nothing)) {
						return A2($MacCASOutreach$graphicsvg$GraphicSVG$AlphaMask, ptrn, inside);
					} else {
						var ptrnlnd = A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, reg);
						var ptrnoutln = A2($MacCASOutreach$graphicsvg$GraphicSVG$Clip, inside, ptrnlnd);
						var newshp = A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh);
						var shpoutln = A2($MacCASOutreach$graphicsvg$GraphicSVG$Clip, inside, newshp);
						return A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$AlphaMask,
							ptrn,
							$MacCASOutreach$graphicsvg$GraphicSVG$Group(
								_List_fromArray(
									[
										inside,
										$MacCASOutreach$graphicsvg$GraphicSVG$GroupOutline(
										$MacCASOutreach$graphicsvg$GraphicSVG$Group(
											_List_fromArray(
												[shpoutln, ptrnoutln])))
									])));
					}
				case 10:
					var reg = shape.a;
					var sh = shape.b;
					var ptrn = A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, $MacCASOutreach$graphicsvg$GraphicSVG$NoLine, $MacCASOutreach$graphicsvg$GraphicSVG$black, reg);
					var inside = A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, $MacCASOutreach$graphicsvg$GraphicSVG$NoLine, $MacCASOutreach$graphicsvg$GraphicSVG$black, sh);
					if (_Utils_eq(lineStyle, $elm$core$Maybe$Nothing)) {
						return A2($MacCASOutreach$graphicsvg$GraphicSVG$Clip, ptrn, inside);
					} else {
						var ptrnlnd = A3(
							$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
							style,
							outlineClr,
							A2($MacCASOutreach$graphicsvg$GraphicSVG$repaint, $MacCASOutreach$graphicsvg$GraphicSVG$blank, reg));
						var ptrnoutln = A2($MacCASOutreach$graphicsvg$GraphicSVG$Clip, inside, ptrnlnd);
						var newshp = A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh);
						var shpoutln = A2($MacCASOutreach$graphicsvg$GraphicSVG$Clip, inside, newshp);
						return A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$Clip,
							ptrn,
							$MacCASOutreach$graphicsvg$GraphicSVG$Group(
								_List_fromArray(
									[
										inside,
										$MacCASOutreach$graphicsvg$GraphicSVG$GroupOutline(
										$MacCASOutreach$graphicsvg$GraphicSVG$Group(
											_List_fromArray(
												[shpoutln, ptrnoutln])))
									])));
					}
				case 13:
					var s = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$Link,
						s,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 14:
					var userMsg = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$Tap,
						userMsg,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 15:
					var userMsg = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$TapAt,
						userMsg,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 16:
					var userMsg = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$EnterShape,
						userMsg,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 17:
					var userMsg = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$EnterAt,
						userMsg,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 18:
					var userMsg = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$Exit,
						userMsg,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 19:
					var userMsg = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$ExitAt,
						userMsg,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 20:
					var userMsg = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$MouseDown,
						userMsg,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 21:
					var userMsg = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$MouseDownAt,
						userMsg,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 22:
					var userMsg = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$MouseUp,
						userMsg,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 23:
					var userMsg = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$MouseUpAt,
						userMsg,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 24:
					var userMsg = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$MoveOverAt,
						userMsg,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 25:
					var userMsg = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$TouchStart,
						userMsg,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 26:
					var userMsg = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$TouchEnd,
						userMsg,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 27:
					var userMsg = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$TouchStartAt,
						userMsg,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 28:
					var userMsg = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$TouchEndAt,
						userMsg,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 29:
					var userMsg = shape.a;
					var sh = shape.b;
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$TouchMoveAt,
						userMsg,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$addOutline, style, outlineClr, sh));
				case 1:
					var w = shape.a;
					var h = shape.b;
					var htm = shape.c;
					return A3($MacCASOutreach$graphicsvg$GraphicSVG$ForeignObject, w, h, htm);
				case 11:
					return $MacCASOutreach$graphicsvg$GraphicSVG$Everything;
				case 12:
					return $MacCASOutreach$graphicsvg$GraphicSVG$Notathing;
				default:
					var s = shape.a;
					var th = shape.b;
					var clr = shape.c;
					return A3($MacCASOutreach$graphicsvg$GraphicSVG$GraphPaper, s, th, clr);
			}
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Circle = function (a) {
	return {$: 0, a: a};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$circle = function (r) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$Circle(r);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$collage = F3(
	function (w, h, shapes) {
		return A3($MacCASOutreach$graphicsvg$GraphicSVG$Collage, w, h, shapes);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$BezierPath = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$curveListHelper = function (_v0) {
	var _v1 = _v0.a;
	var a = _v1.a;
	var b = _v1.b;
	var _v2 = _v0.b;
	var c = _v2.a;
	var d = _v2.b;
	return _Utils_Tuple2(
		_Utils_Tuple2(a, b),
		_Utils_Tuple2(c, d));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$curve = F2(
	function (_v0, list) {
		var a = _v0.a;
		var b = _v0.b;
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$BezierPath,
			_Utils_Tuple2(a, b),
			A2($elm$core$List$map, $MacCASOutreach$graphicsvg$GraphicSVG$curveListHelper, list));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$darkBlue = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 32, 74, 135, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$darkGreen = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 78, 154, 6, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$darkRed = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 164, 0, 0, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$findChroma = F2(
	function (lit, sat) {
		return (1 - $elm$core$Basics$abs((2 * lit) - 1)) * sat;
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$findM = F2(
	function (lit, chroma) {
		return lit - (0.5 * chroma);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$mapTriple = F2(
	function (f, _v0) {
		var a1 = _v0.a;
		var a2 = _v0.b;
		var a3 = _v0.c;
		return _Utils_Tuple3(
			f(a1),
			f(a2),
			f(a3));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$modFloat = F2(
	function (x, m) {
		return x - (m * $elm$core$Basics$floor(x / m));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$findHue_ = function (hue) {
	return hue / $elm$core$Basics$degrees(60);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$findX = F2(
	function (chroma, hue) {
		return chroma * (1 - $elm$core$Basics$abs(
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$modFloat,
				$MacCASOutreach$graphicsvg$GraphicSVG$findHue_(hue),
				2) - 1));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$toRGB_ = F3(
	function (hue, sat, lit) {
		var hue_ = $MacCASOutreach$graphicsvg$GraphicSVG$findHue_(hue);
		var chroma = A2($MacCASOutreach$graphicsvg$GraphicSVG$findChroma, lit, sat);
		var x = A2($MacCASOutreach$graphicsvg$GraphicSVG$findX, chroma, hue);
		return ((hue_ >= 0) && (hue_ < 1)) ? _Utils_Tuple3(chroma, x, 0) : (((hue_ >= 1) && (hue_ < 2)) ? _Utils_Tuple3(x, chroma, 0) : (((hue_ >= 2) && (hue_ < 3)) ? _Utils_Tuple3(0, chroma, x) : (((hue_ >= 3) && (hue_ < 4)) ? _Utils_Tuple3(0, x, chroma) : (((hue_ >= 4) && (hue_ < 5)) ? _Utils_Tuple3(x, 0, chroma) : (((hue_ >= 5) && (hue_ < 6)) ? _Utils_Tuple3(chroma, 0, x) : _Utils_Tuple3(0, 0, 0))))));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$convert = F3(
	function (hue, sat, lit) {
		var hue_ = A2($MacCASOutreach$graphicsvg$GraphicSVG$modFloat, hue, 2 * $elm$core$Basics$pi);
		var rgb_ = A3($MacCASOutreach$graphicsvg$GraphicSVG$toRGB_, hue_, sat, lit);
		var chroma = A2($MacCASOutreach$graphicsvg$GraphicSVG$findChroma, lit, sat);
		var m = A2($MacCASOutreach$graphicsvg$GraphicSVG$findM, lit, chroma);
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$mapTriple,
			function (x) {
				return x * 255;
			},
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$mapTriple,
				function (x) {
					return x + m;
				},
				rgb_));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$hsl = F3(
	function (h, s, l) {
		var _v0 = A3($MacCASOutreach$graphicsvg$GraphicSVG$convert, h, s, l);
		var r = _v0.a;
		var g = _v0.b;
		var b = _v0.c;
		return A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, r, g, b, 1);
	});
var $author$project$Main$fiveAccent = function (model) {
	return A3(
		$MacCASOutreach$graphicsvg$GraphicSVG$hsl,
		$elm$core$Basics$degrees(355),
		model.e,
		0.75);
};
var $author$project$Main$fiveColour = function (model) {
	return A3(
		$MacCASOutreach$graphicsvg$GraphicSVG$hsl,
		$elm$core$Basics$degrees(0),
		model.e,
		0.9);
};
var $author$project$Main$fourAccent = function (model) {
	return A3(
		$MacCASOutreach$graphicsvg$GraphicSVG$hsl,
		$elm$core$Basics$degrees(130),
		model.f,
		0.6);
};
var $author$project$Main$fourColour = function (model) {
	return A3(
		$MacCASOutreach$graphicsvg$GraphicSVG$hsl,
		$elm$core$Basics$degrees(135),
		model.f,
		0.9);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent = F2(
	function (alpha, shape) {
		switch (shape.$) {
			case 1:
				var w = shape.a;
				var h = shape.b;
				var htm = shape.c;
				return A3($MacCASOutreach$graphicsvg$GraphicSVG$ForeignObject, w, h, htm);
			case 2:
				var s = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Move,
					s,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 3:
				var r = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Rotate,
					r,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 4:
				var sx = shape.a;
				var sy = shape.b;
				var sh = shape.c;
				return A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$Scale,
					sx,
					sy,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 5:
				var skx = shape.a;
				var sky = shape.b;
				var sh = shape.c;
				return A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$Skew,
					skx,
					sky,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 6:
				var tm = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Transformed,
					tm,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 7:
				var list = shape.a;
				return $MacCASOutreach$graphicsvg$GraphicSVG$Group(
					A2(
						$elm$core$List$map,
						$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent(alpha),
						list));
			case 8:
				var cmbndshp = shape.a;
				return $MacCASOutreach$graphicsvg$GraphicSVG$GroupOutline(
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, cmbndshp));
			case 13:
				var s = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Link,
					s,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 9:
				var reg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$AlphaMask,
					reg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 10:
				var reg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Clip,
					reg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 11:
				return $MacCASOutreach$graphicsvg$GraphicSVG$Everything;
			case 12:
				return $MacCASOutreach$graphicsvg$GraphicSVG$Notathing;
			case 14:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Tap,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 15:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TapAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 16:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$EnterShape,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 17:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$EnterAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 18:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Exit,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 19:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$ExitAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 20:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$MouseDown,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 21:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$MouseDownAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 22:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$MouseUp,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 23:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$MouseUpAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 24:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$MoveOverAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 25:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TouchStart,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 26:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TouchEnd,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 27:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TouchStartAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 28:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TouchEndAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 29:
				var userMsg = shape.a;
				var sh = shape.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$TouchMoveAt,
					userMsg,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, alpha, sh));
			case 30:
				var s = shape.a;
				var th = shape.b;
				var _v9 = shape.c;
				var r = _v9.a;
				var g = _v9.b;
				var b = _v9.c;
				var a = _v9.d;
				return A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$GraphPaper,
					s,
					th,
					A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, r, g, b, a * alpha));
			default:
				if (!shape.a.$) {
					if (!shape.b.$) {
						var _v4 = shape.a.a;
						var r = _v4.a;
						var g = _v4.b;
						var b = _v4.c;
						var a = _v4.d;
						var _v5 = shape.b.a;
						var lineType = _v5.a;
						var _v6 = _v5.b;
						var sr = _v6.a;
						var sg = _v6.b;
						var sb = _v6.c;
						var sa = _v6.d;
						var st = shape.c;
						return A3(
							$MacCASOutreach$graphicsvg$GraphicSVG$Inked,
							$elm$core$Maybe$Just(
								A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, r, g, b, a * alpha)),
							$elm$core$Maybe$Just(
								_Utils_Tuple2(
									lineType,
									A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, sr, sg, sb, sa * alpha))),
							st);
					} else {
						var _v7 = shape.a.a;
						var r = _v7.a;
						var g = _v7.b;
						var b = _v7.c;
						var a = _v7.d;
						var _v8 = shape.b;
						var st = shape.c;
						return A3(
							$MacCASOutreach$graphicsvg$GraphicSVG$Inked,
							$elm$core$Maybe$Just(
								A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, r, g, b, a * alpha)),
							$elm$core$Maybe$Nothing,
							st);
					}
				} else {
					if (!shape.b.$) {
						var _v1 = shape.a;
						var _v2 = shape.b.a;
						var lineType = _v2.a;
						var _v3 = _v2.b;
						var sr = _v3.a;
						var sg = _v3.b;
						var sb = _v3.c;
						var sa = _v3.d;
						var st = shape.c;
						return A3(
							$MacCASOutreach$graphicsvg$GraphicSVG$Inked,
							$elm$core$Maybe$Nothing,
							$elm$core$Maybe$Just(
								_Utils_Tuple2(
									lineType,
									A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, sr, sg, sb, sa * alpha))),
							st);
					} else {
						var _v10 = shape.a;
						var _v11 = shape.b;
						var st = shape.c;
						return shape;
					}
				}
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$notifyEnter = F2(
	function (msg, shape) {
		return A2($MacCASOutreach$graphicsvg$GraphicSVG$EnterShape, msg, shape);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$notifyLeave = F2(
	function (msg, shape) {
		return A2($MacCASOutreach$graphicsvg$GraphicSVG$Exit, msg, shape);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseMoveAt = F2(
	function (msg, shape) {
		return A2($MacCASOutreach$graphicsvg$GraphicSVG$MoveOverAt, msg, shape);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$notifyTap = F2(
	function (msg, shape) {
		return A2($MacCASOutreach$graphicsvg$GraphicSVG$Tap, msg, shape);
	});
var $author$project$Main$oneAccent = function (model) {
	return A3(
		$MacCASOutreach$graphicsvg$GraphicSVG$hsl,
		$elm$core$Basics$degrees(22),
		model.g,
		0.6);
};
var $author$project$Main$oneColour = function (model) {
	return A3(
		$MacCASOutreach$graphicsvg$GraphicSVG$hsl,
		$elm$core$Basics$degrees(30),
		model.g,
		0.85);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$orange = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 245, 121, 0, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$outlined = F3(
	function (style, outlineClr, stencil) {
		var lineStyle = function () {
			if (!style.$) {
				return $elm$core$Maybe$Nothing;
			} else {
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(style, outlineClr));
			}
		}();
		return A3($MacCASOutreach$graphicsvg$GraphicSVG$Inked, $elm$core$Maybe$Nothing, lineStyle, stencil);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Polygon = function (a) {
	return {$: 5, a: a};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$polygon = function (ptList) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$Polygon(ptList);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$purple = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 117, 80, 123, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$rightTriangle = F2(
	function (base, height) {
		return $MacCASOutreach$graphicsvg$GraphicSVG$polygon(
			_List_fromArray(
				[
					_Utils_Tuple2(0, 0),
					_Utils_Tuple2(base, 0),
					_Utils_Tuple2(0, height)
				]));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$scale = F2(
	function (s, shape) {
		return A3($MacCASOutreach$graphicsvg$GraphicSVG$Scale, s, s, shape);
	});
var $author$project$Main$sixAccent = function (model) {
	return A3(
		$MacCASOutreach$graphicsvg$GraphicSVG$hsl,
		$elm$core$Basics$degrees(180),
		model.d,
		0.5);
};
var $author$project$Main$sixColour = function (model) {
	return A3(
		$MacCASOutreach$graphicsvg$GraphicSVG$hsl,
		$elm$core$Basics$degrees(180),
		model.d,
		0.85);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$Solid = function (a) {
	return {$: 1, a: a};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$solid = function (th) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$Solid(th);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$AlignLeft = 0;
var $MacCASOutreach$graphicsvg$GraphicSVG$Face = F8(
	function (a, b, c, d, e, f, g, h) {
		return {$: 0, a: a, b: b, c: c, d: d, e: e, f: f, g: g, h: h};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$Serif = {$: 0};
var $MacCASOutreach$graphicsvg$GraphicSVG$Text = F2(
	function (a, b) {
		return {$: 7, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$text = function (str) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$Text,
		A8($MacCASOutreach$graphicsvg$GraphicSVG$Face, 12, false, false, false, false, false, $MacCASOutreach$graphicsvg$GraphicSVG$Serif, 0),
		str);
};
var $author$project$Main$threeAccent = function (model) {
	return A3(
		$MacCASOutreach$graphicsvg$GraphicSVG$hsl,
		$elm$core$Basics$degrees(245),
		model.h,
		0.6);
};
var $author$project$Main$threeColour = function (model) {
	return A3(
		$MacCASOutreach$graphicsvg$GraphicSVG$hsl,
		$elm$core$Basics$degrees(250),
		model.h,
		0.9);
};
var $author$project$Main$twoAccent = function (model) {
	return A3(
		$MacCASOutreach$graphicsvg$GraphicSVG$hsl,
		$elm$core$Basics$degrees(280),
		model.i,
		0.6);
};
var $author$project$Main$twoColour = function (model) {
	return A3(
		$MacCASOutreach$graphicsvg$GraphicSVG$hsl,
		$elm$core$Basics$degrees(285),
		model.i,
		0.85);
};
var $author$project$ArcCreator$TransM = function (a) {
	return {$: 6, a: a};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$FixedWidth = {$: 2};
var $MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth = function (stencil) {
	if (stencil.$ === 7) {
		var _v1 = stencil.a;
		var si = _v1.a;
		var bo = _v1.b;
		var i = _v1.c;
		var u = _v1.d;
		var s = _v1.e;
		var sel = _v1.f;
		var f = _v1.g;
		var c = _v1.h;
		var str = stencil.b;
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$Text,
			A8($MacCASOutreach$graphicsvg$GraphicSVG$Face, si, bo, i, u, s, sel, $MacCASOutreach$graphicsvg$GraphicSVG$FixedWidth, c),
			str);
	} else {
		var a = stencil;
		return a;
	}
};
var $MacCASOutreach$graphicsvg$GraphicSVG$size = F2(
	function (sze, stencil) {
		if (stencil.$ === 7) {
			var _v1 = stencil.a;
			var si = _v1.a;
			var bo = _v1.b;
			var i = _v1.c;
			var u = _v1.d;
			var s = _v1.e;
			var sel = _v1.f;
			var f = _v1.g;
			var c = _v1.h;
			var str = stencil.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$Text,
				A8($MacCASOutreach$graphicsvg$GraphicSVG$Face, sze, bo, i, u, s, sel, f, c),
				str);
		} else {
			var a = stencil;
			return a;
		}
	});
var $author$project$ArcCreator$brackets = function (model) {
	var layoutPulls = F3(
		function (_v0, pulls, offsets) {
			var start = _v0.a;
			var end = _v0.b;
			var _v1 = _Utils_Tuple2(pulls, offsets);
			if (_v1.a.b && _v1.b.b) {
				var _v2 = _v1.a;
				var _v3 = _v2.a;
				var _v4 = _v3.a;
				var x1 = _v4.a;
				var y1 = _v4.b;
				var _v5 = _v3.b;
				var x2 = _v5.a;
				var y2 = _v5.b;
				var rest = _v2.b;
				var _v6 = _v1.b;
				var _v7 = _v6.a;
				var x = _v7.a;
				var y = _v7.b;
				var moreOffsets = _v6.b;
				return _Utils_ap(
					_List_fromArray(
						[
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(x, y),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$black,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$size,
									13,
									$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
										$MacCASOutreach$graphicsvg$GraphicSVG$text(start + 'Pull ( '))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(x + 52, y),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$black,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$size,
									13,
									$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
										$MacCASOutreach$graphicsvg$GraphicSVG$text(
											$elm$core$String$fromFloat(x1)))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(x + 84, y),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$black,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$size,
									13,
									$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
										$MacCASOutreach$graphicsvg$GraphicSVG$text(','))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(x + 95, y),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$black,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$size,
									13,
									$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
										$MacCASOutreach$graphicsvg$GraphicSVG$text(
											$elm$core$String$fromFloat(y1)))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(x + 115, y),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$black,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$size,
									13,
									$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
										$MacCASOutreach$graphicsvg$GraphicSVG$text(' )'))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(x + 130, y),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$black,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$size,
									13,
									$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
										$MacCASOutreach$graphicsvg$GraphicSVG$text(' ('))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(x + 145, y),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$black,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$size,
									13,
									$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
										$MacCASOutreach$graphicsvg$GraphicSVG$text(
											$elm$core$String$fromFloat(x2)))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(x + 177, y),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$black,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$size,
									13,
									$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
										$MacCASOutreach$graphicsvg$GraphicSVG$text(','))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(x + 188, y),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$black,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$size,
									13,
									$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
										$MacCASOutreach$graphicsvg$GraphicSVG$text(
											$elm$core$String$fromFloat(y2)))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(x + 208, y),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$black,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$size,
									13,
									$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
										$MacCASOutreach$graphicsvg$GraphicSVG$text(' )' + end)))))
						]),
					A3(
						layoutPulls,
						($elm$core$List$length(rest) === 1) ? _Utils_Tuple2(',', ']') : _Utils_Tuple2(',', ''),
						rest,
						moreOffsets));
			} else {
				return _List_Nil;
			}
		});
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-250, 167),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						13,
						$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
							$MacCASOutreach$graphicsvg$GraphicSVG$text('curve ( '))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-198, 167),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						13,
						$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
							$MacCASOutreach$graphicsvg$GraphicSVG$text(
								$elm$core$String$fromFloat(model.N)))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-166, 167),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						13,
						$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
							$MacCASOutreach$graphicsvg$GraphicSVG$text(','))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-155, 167),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						13,
						$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
							$MacCASOutreach$graphicsvg$GraphicSVG$text(
								$elm$core$String$fromFloat(model.O)))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-135, 167),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						13,
						$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
							$MacCASOutreach$graphicsvg$GraphicSVG$text(' )'))))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					layoutPulls,
					($elm$core$List$length(model.t) === 1) ? _Utils_Tuple2('[', ']') : _Utils_Tuple2('[', ''),
					model.t,
					_List_fromArray(
						[
							_Utils_Tuple2(-230, 147),
							_Utils_Tuple2(-230, 127),
							_Utils_Tuple2(-230, 107),
							_Utils_Tuple2(-230, 87),
							_Utils_Tuple2(-230, 67),
							_Utils_Tuple2(-230, 47),
							_Utils_Tuple2(-230, 27),
							_Utils_Tuple2(-230, 7)
						])))
			]));
};
var $author$project$ArcCreator$code = function (str) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$filled,
		$MacCASOutreach$graphicsvg$GraphicSVG$black,
		A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$size,
			7,
			$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
				$MacCASOutreach$graphicsvg$GraphicSVG$text(str))));
};
var $author$project$ArcCreator$BDown = 10;
var $author$project$ArcCreator$BUp = 9;
var $author$project$ArcCreator$ButtonDown = function (a) {
	return {$: 11, a: a};
};
var $author$project$ArcCreator$GDown = 8;
var $author$project$ArcCreator$GUp = 7;
var $author$project$ArcCreator$RDown = 6;
var $author$project$ArcCreator$RUp = 5;
var $author$project$ArcCreator$SetColour = function (a) {
	return {$: 4, a: a};
};
var $author$project$ArcCreator$clrString = F2(
	function (m, clr) {
		return '(rgb ' + ($elm$core$String$fromFloat(m.G) + (' ' + ($elm$core$String$fromFloat(m.D) + (' ' + ($elm$core$String$fromFloat(m.C) + ')')))));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$ssa = function (n) {
	return A3($elm$core$Basics$clamp, 0, 1, n);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$rgba = F4(
	function (r, g, b, a) {
		return A4(
			$MacCASOutreach$graphicsvg$GraphicSVG$RGBA,
			$MacCASOutreach$graphicsvg$GraphicSVG$ssc(r),
			$MacCASOutreach$graphicsvg$GraphicSVG$ssc(g),
			$MacCASOutreach$graphicsvg$GraphicSVG$ssc(b),
			$MacCASOutreach$graphicsvg$GraphicSVG$ssa(a));
	});
var $author$project$ArcCreator$colourTime = F5(
	function (model, ss, w, h, shape) {
		return _Utils_eq(ss, model.bj) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4(
						$MacCASOutreach$graphicsvg$GraphicSVG$rgba,
						117,
						184,
						135,
						0.6 + (0.4 * $elm$core$Basics$sin((5 * model.aO) - 1))),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h)),
					shape
				])) : shape;
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$italic = function (stencil) {
	if (stencil.$ === 7) {
		var _v1 = stencil.a;
		var si = _v1.a;
		var bo = _v1.b;
		var i = _v1.c;
		var u = _v1.d;
		var s = _v1.e;
		var sel = _v1.f;
		var f = _v1.g;
		var c = _v1.h;
		var str = stencil.b;
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$Text,
			A8($MacCASOutreach$graphicsvg$GraphicSVG$Face, si, bo, true, u, s, sel, f, c),
			str);
	} else {
		var a = stencil;
		return a;
	}
};
var $MacCASOutreach$graphicsvg$GraphicSVG$lightGrey = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 238, 238, 236, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown = F2(
	function (msg, shape) {
		return A2($MacCASOutreach$graphicsvg$GraphicSVG$MouseDown, msg, shape);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp = F2(
	function (msg, shape) {
		return A2($MacCASOutreach$graphicsvg$GraphicSVG$MouseUp, msg, shape);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$rotate = F2(
	function (theta, shape) {
		return A2($MacCASOutreach$graphicsvg$GraphicSVG$Rotate, theta, shape);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$serif = function (stencil) {
	if (stencil.$ === 7) {
		var _v1 = stencil.a;
		var si = _v1.a;
		var bo = _v1.b;
		var i = _v1.c;
		var u = _v1.d;
		var s = _v1.e;
		var sel = _v1.f;
		var f = _v1.g;
		var c = _v1.h;
		var str = stencil.b;
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$Text,
			A8($MacCASOutreach$graphicsvg$GraphicSVG$Face, si, bo, i, u, s, sel, $MacCASOutreach$graphicsvg$GraphicSVG$Serif, c),
			str);
	} else {
		var a = stencil;
		return a;
	}
};
var $author$project$ArcCreator$titleColour = A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 100, 175, 120);
var $elm$core$Basics$turns = function (angleInTurns) {
	return (2 * $elm$core$Basics$pi) * angleInTurns;
};
var $MacCASOutreach$graphicsvg$GraphicSVG$ptOnCircle = F3(
	function (r, n, cn) {
		var angle = $elm$core$Basics$turns(cn / n);
		return _Utils_Tuple2(
			r * $elm$core$Basics$cos(angle),
			r * $elm$core$Basics$sin(angle));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$ngon = F2(
	function (n, r) {
		return $MacCASOutreach$graphicsvg$GraphicSVG$Polygon(
			A2(
				$elm$core$List$map,
				A2(
					$elm$core$Basics$composeL,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$ptOnCircle, r, n),
					$elm$core$Basics$toFloat),
				A2($elm$core$List$range, 0, n)));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$triangle = function (r) {
	return A2($MacCASOutreach$graphicsvg$GraphicSVG$ngon, 3, r);
};
var $author$project$ArcCreator$colours = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-35, -15),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 140, 50)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, 14),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 75, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-75, 11),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$ArcCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('2.Colour!')))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(2, -8),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
					_List_fromArray(
						[
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$ArcCreator$TransM(
								function (m) {
									return _Utils_update(
										m,
										{
											G: (m.G < 248) ? (m.G + 1) : m.G
										});
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
								$author$project$ArcCreator$ButtonDown(4),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
									$author$project$ArcCreator$ButtonDown(5),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-95, -10),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(-30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 255, 10, 10),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$ArcCreator$TransM(
								function (m) {
									return _Utils_update(
										m,
										{
											G: (m.G > 7) ? (m.G - 1) : m.G
										});
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
								$author$project$ArcCreator$ButtonDown(4),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
									$author$project$ArcCreator$ButtonDown(6),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-84, -9),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 180, 140, 140),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$ArcCreator$TransM(
								function (m) {
									return _Utils_update(
										m,
										{
											D: (m.D < 248) ? (m.D + 1) : m.D
										});
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
								$author$project$ArcCreator$ButtonDown(4),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
									$author$project$ArcCreator$ButtonDown(7),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-65, -10),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(-30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 10, 255, 10),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$ArcCreator$TransM(
								function (m) {
									return _Utils_update(
										m,
										{
											D: (m.D > 7) ? (m.D - 1) : m.D
										});
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
								$author$project$ArcCreator$ButtonDown(4),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
									$author$project$ArcCreator$ButtonDown(8),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-54, -9),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 140, 180, 140),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$ArcCreator$TransM(
								function (m) {
									return _Utils_update(
										m,
										{
											C: (m.C < 248) ? (m.C + 1) : m.C
										});
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
								$author$project$ArcCreator$ButtonDown(4),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
									$author$project$ArcCreator$ButtonDown(9),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-35, -10),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(-30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 10, 10, 255),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$ArcCreator$TransM(
								function (m) {
									return _Utils_update(
										m,
										{
											C: (m.C > 7) ? (m.C - 1) : m.C
										});
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
								$author$project$ArcCreator$ButtonDown(4),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
									$author$project$ArcCreator$ButtonDown(10),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-24, -9),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 140, 140, 180),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8)))))))
						]))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (ss, y) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-40, y),
								A5(
									$author$project$ArcCreator$colourTime,
									model,
									ss,
									130,
									10,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-63, -2.5),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											$author$project$ArcCreator$SetColour(ss),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$black,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$size,
													10,
													$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
														$MacCASOutreach$graphicsvg$GraphicSVG$text(
															A2($author$project$ArcCreator$clrString, model, ss)))))))));
						}),
					_List_fromArray(
						[0]),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-10) * x;
						},
						A2($elm$core$List$range, 0, 40))))
			]));
};
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$graphPaperCustom = F3(
	function (s, th, c) {
		return A3($MacCASOutreach$graphicsvg$GraphicSVG$GraphPaper, s, th, c);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$green = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 115, 210, 22, 1);
var $author$project$ArcCreator$CurveDown = 1;
var $author$project$ArcCreator$CurveLeft = 2;
var $author$project$ArcCreator$CurveRight = 3;
var $author$project$ArcCreator$CurveUp = 0;
var $author$project$ArcCreator$Down = 1;
var $author$project$ArcCreator$Left = 2;
var $author$project$ArcCreator$MovePoint = function (a) {
	return {$: 10, a: a};
};
var $author$project$ArcCreator$Right = 3;
var $author$project$ArcCreator$Up = 0;
var $author$project$ArcCreator$keypad = F2(
	function (colour, model) {
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			model.bp,
			$MacCASOutreach$graphicsvg$GraphicSVG$group(
				_List_fromArray(
					[
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
						$author$project$ArcCreator$ButtonDown(4),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
							$author$project$ArcCreator$ButtonDown(0),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$ArcCreator$MovePoint(0),
								A3(
									$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
									$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
									colour,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										colour,
										$MacCASOutreach$graphicsvg$GraphicSVG$polygon(
											_List_fromArray(
												[
													_Utils_Tuple2(0, 0),
													_Utils_Tuple2(80, 0),
													_Utils_Tuple2(40, 10),
													_Utils_Tuple2(0, 0)
												]))))))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
						$author$project$ArcCreator$ButtonDown(4),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
							$author$project$ArcCreator$ButtonDown(2),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$ArcCreator$MovePoint(2),
								A3(
									$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
									$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
									colour,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										colour,
										$MacCASOutreach$graphicsvg$GraphicSVG$polygon(
											_List_fromArray(
												[
													_Utils_Tuple2(0, 0),
													_Utils_Tuple2(0, -12),
													_Utils_Tuple2(-10, -6),
													_Utils_Tuple2(0, 0)
												]))))))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
						$author$project$ArcCreator$ButtonDown(4),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
							$author$project$ArcCreator$ButtonDown(1),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$ArcCreator$MovePoint(1),
								A3(
									$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
									$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
									colour,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										colour,
										$MacCASOutreach$graphicsvg$GraphicSVG$polygon(
											_List_fromArray(
												[
													_Utils_Tuple2(0, -12),
													_Utils_Tuple2(80, -12),
													_Utils_Tuple2(40, -22),
													_Utils_Tuple2(0, -12)
												]))))))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
						$author$project$ArcCreator$ButtonDown(4),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
							$author$project$ArcCreator$ButtonDown(3),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$ArcCreator$MovePoint(3),
								A3(
									$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
									$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
									colour,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										colour,
										$MacCASOutreach$graphicsvg$GraphicSVG$polygon(
											_List_fromArray(
												[
													_Utils_Tuple2(80, -12),
													_Utils_Tuple2(80, 0),
													_Utils_Tuple2(90, -6)
												])))))))
					])));
	});
var $author$project$ArcCreator$invisibleRectangle = F3(
	function (x, y, b) {
		return $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(x, y),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						b,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 12)))
				]));
	});
var $author$project$ArcCreator$magicRect = function (model) {
	var len = $elm$core$List$length(model.t);
	var invisRectLocation = _List_fromArray(
		[
			_Utils_Tuple3(
			_Utils_Tuple2(-201, 176),
			_Utils_Tuple2(-161, 170),
			_Utils_Tuple2(0, len > 0)),
			_Utils_Tuple3(
			_Utils_Tuple2(-181, 156),
			_Utils_Tuple2(-141, 150),
			_Utils_Tuple2(1, len > 0)),
			_Utils_Tuple3(
			_Utils_Tuple2(-91, 156),
			_Utils_Tuple2(-51, 150),
			_Utils_Tuple2(2, len > 0)),
			_Utils_Tuple3(
			_Utils_Tuple2(-181, 136),
			_Utils_Tuple2(-141, 130),
			_Utils_Tuple2(3, len > 1)),
			_Utils_Tuple3(
			_Utils_Tuple2(-91, 136),
			_Utils_Tuple2(-51, 130),
			_Utils_Tuple2(4, len > 1)),
			_Utils_Tuple3(
			_Utils_Tuple2(-181, 116),
			_Utils_Tuple2(-141, 110),
			_Utils_Tuple2(5, len > 2)),
			_Utils_Tuple3(
			_Utils_Tuple2(-91, 116),
			_Utils_Tuple2(-51, 110),
			_Utils_Tuple2(6, len > 2)),
			_Utils_Tuple3(
			_Utils_Tuple2(-181, 96),
			_Utils_Tuple2(-141, 90),
			_Utils_Tuple2(7, len > 3)),
			_Utils_Tuple3(
			_Utils_Tuple2(-91, 96),
			_Utils_Tuple2(-51, 90),
			_Utils_Tuple2(8, len > 3)),
			_Utils_Tuple3(
			_Utils_Tuple2(-181, 76),
			_Utils_Tuple2(-141, 70),
			_Utils_Tuple2(9, len > 4)),
			_Utils_Tuple3(
			_Utils_Tuple2(-91, 76),
			_Utils_Tuple2(-51, 70),
			_Utils_Tuple2(10, len > 4)),
			_Utils_Tuple3(
			_Utils_Tuple2(-181, 56),
			_Utils_Tuple2(-141, 50),
			_Utils_Tuple2(11, len > 5)),
			_Utils_Tuple3(
			_Utils_Tuple2(-91, 56),
			_Utils_Tuple2(-51, 50),
			_Utils_Tuple2(12, len > 5)),
			_Utils_Tuple3(
			_Utils_Tuple2(-181, 36),
			_Utils_Tuple2(-141, 30),
			_Utils_Tuple2(13, len > 6)),
			_Utils_Tuple3(
			_Utils_Tuple2(-91, 36),
			_Utils_Tuple2(-51, 30),
			_Utils_Tuple2(14, len > 6))
		]);
	return A2(
		$elm$core$List$map,
		function (_v0) {
			var _v1 = _v0.a;
			var a = _v1.a;
			var b = _v1.b;
			var _v2 = _v0.b;
			var c = _v2.a;
			var d = _v2.b;
			var _v3 = _v0.c;
			var e = _v3.a;
			var f = _v3.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
				$author$project$ArcCreator$TransM(
					function (m) {
						return _Utils_update(
							m,
							{
								bp: f ? _Utils_Tuple2(a, b) : m.bp,
								H: f ? e : m.H
							});
					}),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
					(_Utils_eq(model.H, e) && f) ? ($elm$core$Basics$abs(
						$elm$core$Basics$sin(model.aO)) * 0.5) : 0,
					A3(
						$author$project$ArcCreator$invisibleRectangle,
						c,
						d,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 117, 255, 135))));
		},
		invisRectLocation);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$red = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 204, 0, 0, 1);
var $author$project$ArcCreator$colourFun = function (m) {
	var _v0 = m.bj;
	return A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, m.G, m.D, m.C);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$Broken = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$dashed = function (th) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$Broken,
		_List_fromArray(
			[
				_Utils_Tuple2(th * 5, th * 2.5)
			]),
		th);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$dotdash = function (th) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$Broken,
		_List_fromArray(
			[
				_Utils_Tuple2(th, th),
				_Utils_Tuple2(th * 5, th)
			]),
		th);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$dotted = function (th) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$Broken,
		_List_fromArray(
			[
				_Utils_Tuple2(th, th)
			]),
		th);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$longdash = function (th) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$Broken,
		_List_fromArray(
			[
				_Utils_Tuple2(th * 12, th * 6)
			]),
		th);
};
var $author$project$ArcCreator$lineStyleFun = function (m) {
	var _v0 = m.br;
	switch (_v0) {
		case 0:
			return $MacCASOutreach$graphicsvg$GraphicSVG$solid(m.I);
		case 1:
			return $MacCASOutreach$graphicsvg$GraphicSVG$dotted(m.I);
		case 2:
			return $MacCASOutreach$graphicsvg$GraphicSVG$dashed(m.I);
		case 3:
			return $MacCASOutreach$graphicsvg$GraphicSVG$longdash(m.I);
		default:
			return $MacCASOutreach$graphicsvg$GraphicSVG$dotdash(m.I);
	}
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$createTopLevelList = function (_v0) {
	var _v1 = _v0.a;
	var a = _v1.a;
	var b = _v1.b;
	var _v2 = _v0.b;
	var c = _v2.a;
	var d = _v2.b;
	return _List_fromArray(
		[
			_Utils_Tuple2(a, b),
			_Utils_Tuple2(c, d)
		]);
};
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (!_v0.$) {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$generateCHCircle = F2(
	function (ar, _int) {
		var p1 = function () {
			var _v0 = A2($elm$core$Array$get, _int, ar);
			if (!_v0.$) {
				var p = _v0.a;
				return p;
			} else {
				return _Utils_Tuple2(0, 0);
			}
		}();
		var ptStr = $MacCASOutreach$graphicsvg$GraphicSVG$pairToString(p1);
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			p1,
			$MacCASOutreach$graphicsvg$GraphicSVG$group(
				_List_fromArray(
					[
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$red,
						$MacCASOutreach$graphicsvg$GraphicSVG$circle(2)),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(5, 5),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$black,
							$MacCASOutreach$graphicsvg$GraphicSVG$text('(' + (ptStr + ')'))))
					])));
	});
var $elm$core$Array$length = function (_v0) {
	var len = _v0.a;
	return len;
};
var $MacCASOutreach$graphicsvg$GraphicSVG$generateCHCircles = function (ar) {
	var len = $elm$core$Array$length(ar);
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		A2(
			$elm$core$List$map,
			$MacCASOutreach$graphicsvg$GraphicSVG$generateCHCircle(ar),
			A2($elm$core$List$range, 0, len - 1)));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$Path = function (a) {
	return {$: 6, a: a};
};
var $MacCASOutreach$graphicsvg$GraphicSVG$line = F2(
	function (p1, p2) {
		return $MacCASOutreach$graphicsvg$GraphicSVG$Path(
			_List_fromArray(
				[p1, p2]));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$generateCHLine = F2(
	function (ar, _int) {
		var p2 = function () {
			var _v1 = A2($elm$core$Array$get, _int + 1, ar);
			if (!_v1.$) {
				var p = _v1.a;
				return p;
			} else {
				return _Utils_Tuple2(0, 0);
			}
		}();
		var p1 = function () {
			var _v0 = A2($elm$core$Array$get, _int, ar);
			if (!_v0.$) {
				var p = _v0.a;
				return p;
			} else {
				return _Utils_Tuple2(0, 0);
			}
		}();
		return A3(
			$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
			$MacCASOutreach$graphicsvg$GraphicSVG$dashed(0.5),
			$MacCASOutreach$graphicsvg$GraphicSVG$black,
			A2($MacCASOutreach$graphicsvg$GraphicSVG$line, p1, p2));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$generateCHLines = function (ar) {
	var len = $elm$core$Array$length(ar);
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		A2(
			$elm$core$List$map,
			$MacCASOutreach$graphicsvg$GraphicSVG$generateCHLine(ar),
			A2($elm$core$List$range, 0, len - 2)));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$generateCurveHelper = F2(
	function (_v0, list) {
		var a = _v0.a;
		var b = _v0.b;
		var l1Array = $elm$core$Array$fromList(
			_Utils_ap(
				_List_fromArray(
					[
						_Utils_Tuple2(a, b)
					]),
				$elm$core$List$concat(
					A2($elm$core$List$map, $MacCASOutreach$graphicsvg$GraphicSVG$createTopLevelList, list))));
		return $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					$MacCASOutreach$graphicsvg$GraphicSVG$generateCHLines(l1Array),
					$MacCASOutreach$graphicsvg$GraphicSVG$generateCHCircles(l1Array)
				]));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$curveHelper = function (shape) {
	switch (shape.$) {
		case 2:
			var s = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$Move,
				s,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 3:
			var r = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$Rotate,
				r,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 4:
			var sx = shape.a;
			var sy = shape.b;
			var sh = shape.c;
			return A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$Scale,
				sx,
				sy,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 5:
			var skx = shape.a;
			var sky = shape.b;
			var sh = shape.c;
			return A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$Skew,
				skx,
				sky,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 6:
			var tm = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$Transformed,
				tm,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 7:
			var list = shape.a;
			return $MacCASOutreach$graphicsvg$GraphicSVG$Group(
				A2($elm$core$List$map, $MacCASOutreach$graphicsvg$GraphicSVG$curveHelper, list));
		case 8:
			var cmbndshp = shape.a;
			return $MacCASOutreach$graphicsvg$GraphicSVG$GroupOutline(
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(cmbndshp));
		case 13:
			var s = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$Link,
				s,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 9:
			var reg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$AlphaMask,
				reg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 10:
			var reg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$Clip,
				reg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 14:
			var userMsg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$Tap,
				userMsg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 15:
			var userMsg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$TapAt,
				userMsg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 16:
			var userMsg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$EnterShape,
				userMsg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 17:
			var userMsg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$EnterAt,
				userMsg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 18:
			var userMsg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$Exit,
				userMsg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 19:
			var userMsg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$ExitAt,
				userMsg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 20:
			var userMsg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$MouseDown,
				userMsg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 21:
			var userMsg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$MouseDownAt,
				userMsg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 22:
			var userMsg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$MouseUp,
				userMsg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 23:
			var userMsg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$MouseUpAt,
				userMsg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 24:
			var userMsg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$MoveOverAt,
				userMsg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 25:
			var userMsg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$TouchStart,
				userMsg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 26:
			var userMsg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$TouchEnd,
				userMsg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 27:
			var userMsg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$TouchStartAt,
				userMsg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 28:
			var userMsg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$TouchEndAt,
				userMsg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 29:
			var userMsg = shape.a;
			var sh = shape.b;
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$TouchMoveAt,
				userMsg,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(sh));
		case 0:
			if (shape.c.$ === 4) {
				var _v1 = shape.c;
				var _v2 = _v1.a;
				var a = _v2.a;
				var b = _v2.b;
				var list = _v1.b;
				return $MacCASOutreach$graphicsvg$GraphicSVG$group(
					_List_fromArray(
						[
							shape,
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$generateCurveHelper,
							_Utils_Tuple2(a, b),
							list)
						]));
			} else {
				var clr = shape.a;
				var ln = shape.b;
				var sh = shape.c;
				return A3($MacCASOutreach$graphicsvg$GraphicSVG$Inked, clr, ln, sh);
			}
		case 1:
			var w = shape.a;
			var h = shape.b;
			var htm = shape.c;
			return A3($MacCASOutreach$graphicsvg$GraphicSVG$ForeignObject, w, h, htm);
		case 11:
			return $MacCASOutreach$graphicsvg$GraphicSVG$Everything;
		case 12:
			return $MacCASOutreach$graphicsvg$GraphicSVG$Notathing;
		default:
			var s = shape.a;
			var th = shape.b;
			var clr = shape.c;
			return A3($MacCASOutreach$graphicsvg$GraphicSVG$GraphPaper, s, th, clr);
	}
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $author$project$ArcCreator$listOfPoints = function (model) {
	return _Utils_ap(
		A3(
			$elm$core$List$map2,
			F2(
				function (pts, idx) {
					return _Utils_Tuple2(pts, idx);
				}),
			A2(
				$elm$core$List$concatMap,
				function (_v0) {
					var p1 = _v0.a;
					var p2 = _v0.b;
					return _List_fromArray(
						[p1, p2]);
				},
				model.t),
			_List_fromArray(
				[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16])),
		_List_fromArray(
			[
				_Utils_Tuple2(
				_Utils_Tuple2(model.N, model.O),
				0)
			]));
};
var $author$project$ArcCreator$flashingCircles = function (model) {
	return A2(
		$elm$core$List$map,
		function (_v0) {
			var _v1 = _v0.a;
			var a = _v1.a;
			var b = _v1.b;
			var c = _v0.b;
			return _Utils_eq(model.H, c) ? A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(a, b),
				A2(
					(A2($elm$core$Basics$modBy, 2, c) === 1) ? $MacCASOutreach$graphicsvg$GraphicSVG$outlined(
						$MacCASOutreach$graphicsvg$GraphicSVG$solid(1)) : $MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 117, 184, 135),
					$MacCASOutreach$graphicsvg$GraphicSVG$circle(
						$elm$core$Basics$abs(
							($elm$core$Basics$sin(model.aO * 2) / 2) + 2)))) : $MacCASOutreach$graphicsvg$GraphicSVG$group(_List_Nil);
		},
		$author$project$ArcCreator$listOfPoints(model));
};
var $author$project$ArcCreator$maybeCurveHelper = F2(
	function (m, x) {
		return m.bm ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
			A2(
				$elm$core$List$cons,
				$MacCASOutreach$graphicsvg$GraphicSVG$curveHelper(x),
				$author$project$ArcCreator$flashingCircles(m))) : $MacCASOutreach$graphicsvg$GraphicSVG$group(
			A2(
				$elm$core$List$cons,
				x,
				$author$project$ArcCreator$flashingCircles(m)));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$scaleX = F2(
	function (s, shape) {
		return A3($MacCASOutreach$graphicsvg$GraphicSVG$Scale, s, 1, shape);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$scaleY = F2(
	function (s, shape) {
		return A3($MacCASOutreach$graphicsvg$GraphicSVG$Scale, 1, s, shape);
	});
var $author$project$ArcCreator$stencilFun = function (m) {
	var _v0 = m.bT;
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$curve,
		_Utils_Tuple2(m.N, m.O),
		A2(
			$elm$core$List$map,
			function (_v1) {
				var _v2 = _v1.a;
				var a = _v2.a;
				var b = _v2.b;
				var _v3 = _v1.b;
				var c = _v3.a;
				var d = _v3.b;
				return A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$Pull,
					_Utils_Tuple2(a, b),
					_Utils_Tuple2(c, d));
			},
			m.t));
};
var $author$project$ArcCreator$shapeFun = function (m) {
	return (m.aZ ? $MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent(m.ad) : function (x) {
		return x;
	})(
		(m.a1 ? $MacCASOutreach$graphicsvg$GraphicSVG$scaleY(m._) : function (x) {
			return x;
		})(
			(m.a0 ? $MacCASOutreach$graphicsvg$GraphicSVG$scaleX(m._) : function (x) {
				return x;
			})(
				(m.a$ ? $MacCASOutreach$graphicsvg$GraphicSVG$scale(m._) : function (x) {
					return x;
				})(
					(m.a_ ? $MacCASOutreach$graphicsvg$GraphicSVG$rotate(
						$elm$core$Basics$degrees(m.aI)) : function (x) {
						return x;
					})(
						(m.b5 ? $MacCASOutreach$graphicsvg$GraphicSVG$move(
							_Utils_Tuple2(m.cv, m.bg)) : function (x) {
							return x;
						})(
							function () {
								var _v0 = m.aV;
								if (!_v0) {
									return A2(
										$author$project$ArcCreator$maybeCurveHelper,
										m,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$filled,
											$author$project$ArcCreator$colourFun(m),
											$author$project$ArcCreator$stencilFun(m)));
								} else {
									return A2(
										$author$project$ArcCreator$maybeCurveHelper,
										m,
										A3(
											$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
											$author$project$ArcCreator$lineStyleFun(m),
											$author$project$ArcCreator$colourFun(m),
											$author$project$ArcCreator$stencilFun(m)));
								}
							}()))))));
};
var $author$project$ArcCreator$Draw = function (a) {
	return {$: 2, a: a};
};
var $author$project$ArcCreator$LStyle = {$: 3};
var $author$project$ArcCreator$Outlined = 1;
var $author$project$ArcCreator$fillItOrOutlineItTime = F5(
	function (model, ss, w, h, shape) {
		return _Utils_eq(ss, model.aV) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4(
						$MacCASOutreach$graphicsvg$GraphicSVG$rgba,
						117,
						184,
						135,
						0.6 + (0.4 * $elm$core$Basics$sin((5 * model.aO) - 0.5))),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h)),
					shape
				])) : shape;
	});
var $author$project$ArcCreator$stampString = F2(
	function (m, stamp) {
		if (!stamp) {
			return 'filled ';
		} else {
			return 'outlined ';
		}
	});
var $author$project$ArcCreator$styleString = function (m) {
	return '(' + (function () {
		var _v0 = m.br;
		switch (_v0) {
			case 0:
				return 'solid ';
			case 1:
				return 'dotted ';
			case 2:
				return 'dashed ';
			case 3:
				return 'longdash ';
			default:
				return 'dotdash ';
		}
	}() + ($elm$core$String$fromFloat(m.I) + ')'));
};
var $author$project$ArcCreator$stamps = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, -1),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 130, 30)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, 14),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 95, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-85, 11),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$ArcCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('4. Fill it or Outline it!')))))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (ss, y) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-40, y),
								A5(
									$author$project$ArcCreator$fillItOrOutlineItTime,
									model,
									ss,
									130,
									10,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-63, -2.5),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											$author$project$ArcCreator$Draw(ss),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$black,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$size,
													10,
													$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
														$MacCASOutreach$graphicsvg$GraphicSVG$text(
															A2($author$project$ArcCreator$stampString, model, ss)))))))));
						}),
					_List_fromArray(
						[0, 1]),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-10) * x;
						},
						A2($elm$core$List$range, 0, 20)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
				$author$project$ArcCreator$LStyle,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-43, -12.5),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$black,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$size,
							10,
							$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
								$MacCASOutreach$graphicsvg$GraphicSVG$text(
									$author$project$ArcCreator$styleString(model)))))))
			]));
};
var $author$project$ArcCreator$stencils = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(22, -77),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 255, 185)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(0, 13),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 75, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-35, 10),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$ArcCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('1.Curve Points'))))))
			]));
};
var $author$project$ArcCreator$CurveHelper = 5;
var $author$project$ArcCreator$MakeTransparent = 4;
var $author$project$ArcCreator$Rotate = 0;
var $author$project$ArcCreator$Scale = 1;
var $author$project$ArcCreator$ScaleX = 2;
var $author$project$ArcCreator$ScaleY = 3;
var $author$project$ArcCreator$Toggle = function (a) {
	return {$: 5, a: a};
};
var $author$project$ArcCreator$applyTransformsTime = F5(
	function (model, t, w, h, shape) {
		return function () {
			switch (t) {
				case 0:
					return model.a_;
				case 1:
					return model.a$;
				case 2:
					return model.a0;
				case 3:
					return model.a1;
				case 4:
					return model.aZ;
				default:
					return model.bm;
			}
		}() ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(375, 53),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4(
							$MacCASOutreach$graphicsvg$GraphicSVG$rgba,
							117,
							184,
							135,
							0.6 + (0.4 * $elm$core$Basics$sin((5 * model.aO) - 1.5))),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h))),
					shape
				])) : shape;
	});
var $author$project$ArcCreator$transformString = F2(
	function (m, t) {
		switch (t) {
			case 0:
				return '|> rotate (degrees ' + ($elm$core$String$fromFloat(m.aI) + ')');
			case 1:
				return '|> scale ' + $elm$core$String$fromFloat(m._);
			case 2:
				return '|> scaleX ' + $elm$core$String$fromFloat(m.aB);
			case 3:
				return '|> scaleY ' + $elm$core$String$fromFloat(m.aC);
			case 4:
				return '|> makeTransparent ' + $elm$core$String$fromFloat(m.ad);
			default:
				return '|> curveHelper ';
		}
	});
var $author$project$ArcCreator$transforms = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(340, 30),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 140, 70)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(340, 68),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 95, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(300, 65),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$ArcCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('3. Apply Transforms!')))))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (ss, y) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-35, y),
								A5(
									$author$project$ArcCreator$applyTransformsTime,
									model,
									ss,
									140,
									10,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(307, 50),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											$author$project$ArcCreator$Toggle(ss),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$black,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$size,
													10,
													$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
														$MacCASOutreach$graphicsvg$GraphicSVG$text(
															A2($author$project$ArcCreator$transformString, model, ss)))))))));
						}),
					_List_fromArray(
						[1, 2, 3, 0, 4, 5]),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-10) * x;
						},
						A2($elm$core$List$range, 0, 20))))
			]));
};
var $author$project$ArcCreator$tweaks = $MacCASOutreach$graphicsvg$GraphicSVG$group(
	_List_fromArray(
		[
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-35, -11),
			A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
				$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
				$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 140, 50)))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-60, 14),
			A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
				$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
				$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$white,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 55, 12)))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-85, 11),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$author$project$ArcCreator$titleColour,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					10,
					$MacCASOutreach$graphicsvg$GraphicSVG$italic(
						$MacCASOutreach$graphicsvg$GraphicSVG$serif(
							$MacCASOutreach$graphicsvg$GraphicSVG$text('5. Tweak it!')))))),
			$MacCASOutreach$graphicsvg$GraphicSVG$group(
			A3(
				$elm$core$List$map2,
				F2(
					function (_v0, _v1) {
						var str = _v0.a;
						var msg = _v0.b;
						var x = _v1.a;
						var y = _v1.b;
						return A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2((-68) + x, (-2.5) + y),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								msg,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$black,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$size,
										10,
										$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
											$MacCASOutreach$graphicsvg$GraphicSVG$text(str))))));
					}),
				_List_fromArray(
					[
						_Utils_Tuple2(
						'clockwise',
						$author$project$ArcCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{aI: m.aI - 30});
							})),
						_Utils_Tuple2(
						'counter',
						$author$project$ArcCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{aI: m.aI + 30});
							})),
						_Utils_Tuple2(
						'thicker',
						$author$project$ArcCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										I: (m.I < 10) ? (m.I + 0.5) : 10
									});
							})),
						_Utils_Tuple2(
						'thinner',
						$author$project$ArcCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										I: (m.I > 0.5) ? (m.I - 0.5) : 0.5
									});
							})),
						_Utils_Tuple2(
						'solider',
						$author$project$ArcCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										ad: (m.ad < 1) ? (m.ad + 0.125) : 1
									});
							})),
						_Utils_Tuple2(
						'ghostier',
						$author$project$ArcCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										ad: (m.ad > 0) ? (m.ad - 0.125) : 0
									});
							})),
						_Utils_Tuple2(
						'bigger',
						$author$project$ArcCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										_: (m._ < 3) ? (m._ + 0.25) : 3,
										aB: (m.aB < 3) ? (m.aB + 0.25) : 3,
										aC: (m.aC < 3) ? (m.aC + 0.25) : 3
									});
							})),
						_Utils_Tuple2(
						'smaller',
						$author$project$ArcCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										_: (_Utils_cmp(m._, -3) > 0) ? (m._ - 0.25) : (-3),
										aB: (_Utils_cmp(m.aB, -3) > 0) ? (m.aB - 0.25) : (-3),
										aC: (_Utils_cmp(m.aC, -3) > 0) ? (m.aC - 0.25) : (-3)
									});
							}))
					]),
				$elm$core$List$concat(
					A2(
						$elm$core$List$map,
						function (idx) {
							return _List_fromArray(
								[
									_Utils_Tuple2(-30, (-10) * idx),
									_Utils_Tuple2(40, (-10) * idx)
								]);
						},
						A2($elm$core$List$range, 0, 20)))))
		]));
var $MacCASOutreach$graphicsvg$GraphicSVG$selectable = function (stencil) {
	if (stencil.$ === 7) {
		var _v1 = stencil.a;
		var si = _v1.a;
		var bo = _v1.b;
		var i = _v1.c;
		var u = _v1.d;
		var s = _v1.e;
		var sel = _v1.f;
		var f = _v1.g;
		var c = _v1.h;
		var str = stencil.b;
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$Text,
			A8($MacCASOutreach$graphicsvg$GraphicSVG$Face, si, bo, i, u, s, true, f, c),
			str);
	} else {
		var a = stencil;
		return a;
	}
};
var $author$project$ShapeCreateAssets$copiable = function (str) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$filled,
		$MacCASOutreach$graphicsvg$GraphicSVG$black,
		A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$size,
			10,
			$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
				$MacCASOutreach$graphicsvg$GraphicSVG$selectable(
					$MacCASOutreach$graphicsvg$GraphicSVG$text(str)))));
};
var $author$project$ShapeCreateAssets$ptCode = function (_v0) {
	var x = _v0.a;
	var y = _v0.b;
	return '(' + ($elm$core$String$fromFloat(x) + (',' + ($elm$core$String$fromFloat(y) + ')')));
};
var $author$project$ShapeCreateAssets$pullCode = function (_v0) {
	var pxy = _v0.a;
	var xy = _v0.b;
	return 'Pull ' + ($author$project$ShapeCreateAssets$ptCode(pxy) + (' ' + $author$project$ShapeCreateAssets$ptCode(xy)));
};
var $author$project$ArcCreator$pullsText = function (model) {
	var pulls = A2($elm$core$List$map, $author$project$ShapeCreateAssets$pullCode, model.t);
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				$author$project$ShapeCreateAssets$copiable(
				'curve ' + ($author$project$ShapeCreateAssets$ptCode(
					_Utils_Tuple2(model.N, model.O)) + (' [' + ($elm$core$String$concat(
					A2($elm$core$List$intersperse, ', ', pulls)) + ' ]'))))
			]));
};
var $author$project$ArcCreator$yourCode = function (m) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$move,
		_Utils_Tuple2(0, 20),
		$MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(0, -30),
					A3(
						$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
						$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
						$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
							A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 490, 85)))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-145, 14),
					A3(
						$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
						$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
						$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$white,
							A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 110, 12)))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-190, 11),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$author$project$ArcCreator$titleColour,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$size,
							10,
							$MacCASOutreach$graphicsvg$GraphicSVG$italic(
								$MacCASOutreach$graphicsvg$GraphicSVG$serif(
									$MacCASOutreach$graphicsvg$GraphicSVG$text('6. Your (copiable) code! Use cmd/ctrl-A, cmd/ctrl-C. ')))))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-240, 0),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$scale,
						0.5,
						$author$project$ArcCreator$pullsText(m))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-220, 0),
					$MacCASOutreach$graphicsvg$GraphicSVG$group(
						_Utils_ap(
							_List_fromArray(
								[
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(0, -10),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$scale,
										0.5,
										$author$project$ShapeCreateAssets$copiable(
											'  |> ' + (A2($author$project$ArcCreator$stampString, m, m.aV) + (((m.aV === 1) ? ($author$project$ArcCreator$styleString(m) + ' ') : '') + A2($author$project$ArcCreator$clrString, m, m.bj))))))
								]),
							A3(
								$elm$core$List$map2,
								F2(
									function (str, y) {
										return A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(0, y),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$scale,
												0.66,
												$author$project$ArcCreator$code(str)));
									}),
								$elm$core$List$concat(
									A2(
										$elm$core$List$map,
										function (_v0) {
											var flag = _v0.a;
											var t = _v0.b;
											return flag ? _List_fromArray(
												[
													'  ' + A2($author$project$ArcCreator$transformString, m, t)
												]) : _List_Nil;
										},
										_List_fromArray(
											[
												_Utils_Tuple2(m.a$, 1),
												_Utils_Tuple2(m.a0, 2),
												_Utils_Tuple2(m.a1, 3),
												_Utils_Tuple2(m.a_, 0),
												_Utils_Tuple2(m.aZ, 4)
											]))),
								_List_fromArray(
									[-20, -30, -40, -50, -60, -70, -80])))))
				])));
};
var $author$project$ArcCreator$view = function (model) {
	return _List_fromArray(
		[
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(0, -30),
			$MacCASOutreach$graphicsvg$GraphicSVG$group(
				_List_fromArray(
					[
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
						0.25,
						A3(
							$MacCASOutreach$graphicsvg$GraphicSVG$graphPaperCustom,
							10,
							1,
							A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 117, 184, 135))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 117, 184, 135),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 512, 0.5)),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 117, 184, 135),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 512)),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(0, 100),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$orange,
							A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 4, 0.5))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(3, 100),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$orange,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$size,
								7,
								$MacCASOutreach$graphicsvg$GraphicSVG$text('(0,100)')))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(0, -100),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$orange,
							A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 4, 0.5))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(3, -100),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$orange,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$size,
								7,
								$MacCASOutreach$graphicsvg$GraphicSVG$text('(0,-100)')))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(-100, 0),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$orange,
							A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(-100, 3),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$orange,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$size,
								7,
								$MacCASOutreach$graphicsvg$GraphicSVG$text('(-100,0)')))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(100, 0),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$orange,
							A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(100, 3),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$orange,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$size,
								7,
								$MacCASOutreach$graphicsvg$GraphicSVG$text('(100,0)')))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(-200, 0),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$orange,
							A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(-200, 3),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$orange,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$size,
								7,
								$MacCASOutreach$graphicsvg$GraphicSVG$text('(-200,0)')))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(200, 0),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$orange,
							A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(200, 3),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$orange,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$size,
								7,
								$MacCASOutreach$graphicsvg$GraphicSVG$text('(200,0)')))),
						$author$project$ArcCreator$shapeFun(model)
					]))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-150, 170),
			$author$project$ArcCreator$stencils(model)),
			$author$project$ArcCreator$brackets(model),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(115, -20),
			$author$project$ArcCreator$code('|>')),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(230, -20),
			$author$project$ArcCreator$stamps(model)),
			$MacCASOutreach$graphicsvg$GraphicSVG$group(
			$author$project$ArcCreator$magicRect(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(110, 165),
			$author$project$ArcCreator$code(' ')),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(230, 169),
			$author$project$ArcCreator$colours(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-150, 30),
			$author$project$ArcCreator$transforms(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(230, -60),
			$author$project$ArcCreator$tweaks),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$scale,
			1.2,
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(35, -150),
				$author$project$ArcCreator$yourCode(model))),
			$MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$author$project$ArcCreator$keypad,
					A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 117, 184, 135, 0.2),
					model)
				])),
			$MacCASOutreach$graphicsvg$GraphicSVG$group(
			($elm$core$List$length(model.t) > 1) ? A2(
				$elm$core$List$map,
				function (idx) {
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
						$author$project$ArcCreator$TransM(
							function (m) {
								var ps = m.t;
								var len = $elm$core$List$length(ps);
								return _Utils_update(
									m,
									{
										bp: _Utils_Tuple2(-181, 156),
										H: 1,
										t: (len > 1) ? _Utils_ap(
											A2($elm$core$List$take, idx - 1, ps),
											A2($elm$core$List$drop, idx, ps)) : ps
									});
							}),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(10, 170 - (20 * idx)),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$scale,
								0.5,
								$MacCASOutreach$graphicsvg$GraphicSVG$group(
									_List_fromArray(
										[
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$filled,
											$MacCASOutreach$graphicsvg$GraphicSVG$red,
											$MacCASOutreach$graphicsvg$GraphicSVG$circle(10)),
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(45),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$white,
												A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 5, 15))),
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(-45),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$white,
												A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 5, 15)))
										])))));
				},
				A2(
					$elm$core$List$range,
					1,
					$elm$core$List$length(model.t))) : _List_Nil),
			$MacCASOutreach$graphicsvg$GraphicSVG$group(
			A2(
				$elm$core$List$map,
				function (idx) {
					return A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
						$author$project$ArcCreator$TransM(
							function (m) {
								var ps = m.t;
								var len = $elm$core$List$length(ps);
								return _Utils_update(
									m,
									{
										t: (len < 7) ? _Utils_ap(
											A2($elm$core$List$take, idx, ps),
											_Utils_ap(
												_List_fromArray(
													[
														_Utils_Tuple2(
														_Utils_Tuple2(0, 0),
														_Utils_Tuple2(0, 0))
													]),
												A2($elm$core$List$drop, idx, ps))) : ps
									});
							}),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(-240, 160 - (20 * idx)),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$scale,
								0.5,
								$MacCASOutreach$graphicsvg$GraphicSVG$group(
									_List_fromArray(
										[
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$filled,
											$MacCASOutreach$graphicsvg$GraphicSVG$green,
											$MacCASOutreach$graphicsvg$GraphicSVG$circle(10)),
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(90),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$white,
												A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 5, 15))),
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$filled,
											$MacCASOutreach$graphicsvg$GraphicSVG$white,
											A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 5, 15))
										])))));
				},
				($elm$core$List$length(model.t) < 7) ? A2(
					$elm$core$List$range,
					0,
					$elm$core$List$length(model.t)) : _List_Nil))
		]);
};
var $author$project$PolygonCreator$AddPoint = {$: 11};
var $author$project$PolygonCreator$DeletePoint = {$: 12};
var $author$project$PolygonCreator$code = function (str) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$filled,
		$MacCASOutreach$graphicsvg$GraphicSVG$black,
		A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$size,
			13,
			$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
				$MacCASOutreach$graphicsvg$GraphicSVG$text(str))));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$grey = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 211, 215, 207, 1);
var $author$project$PolygonCreator$button = function (model) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$move,
		_Utils_Tuple2(-130, 150),
		$MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					$author$project$PolygonCreator$code('More'),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
					$author$project$PolygonCreator$AddPoint,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(13, 4),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
							0,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$grey,
								A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 28, 15))))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(0, -25),
					$MacCASOutreach$graphicsvg$GraphicSVG$group(
						_List_fromArray(
							[
								$author$project$PolygonCreator$code('Less'),
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$PolygonCreator$DeletePoint,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(13, 4),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
										0,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$filled,
											$MacCASOutreach$graphicsvg$GraphicSVG$grey,
											A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 28, 15)))))
							])))
				])));
};
var $author$project$PolygonCreator$clickhere = function (model) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$move,
		_Utils_Tuple2(50, -20),
		$MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-5, 0),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
						$elm$core$Basics$degrees(180),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$red,
							$MacCASOutreach$graphicsvg$GraphicSVG$triangle(5)))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$red,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 15, 2)),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(15, -2),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$black,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$size,
							10,
							$MacCASOutreach$graphicsvg$GraphicSVG$text('Click here!'))))
				])));
};
var $author$project$PolygonCreator$BlueDown = 5;
var $author$project$PolygonCreator$BlueUp = 4;
var $author$project$PolygonCreator$Buttondown = function (a) {
	return {$: 13, a: a};
};
var $author$project$PolygonCreator$GreenDown = 3;
var $author$project$PolygonCreator$GreenUp = 2;
var $author$project$PolygonCreator$RedDown = 1;
var $author$project$PolygonCreator$RedUp = 0;
var $author$project$PolygonCreator$SetColour = function (a) {
	return {$: 3, a: a};
};
var $author$project$PolygonCreator$TransM = function (a) {
	return {$: 5, a: a};
};
var $author$project$PolygonCreator$clrString = F2(
	function (m, clr) {
		return '(rgb ' + ($elm$core$String$fromFloat(m.G) + (' ' + ($elm$core$String$fromFloat(m.D) + (' ' + ($elm$core$String$fromFloat(m.C) + ')')))));
	});
var $author$project$PolygonCreator$time3 = F5(
	function (model, ss, w, h, shape) {
		return _Utils_eq(ss, model.bj) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4(
						$MacCASOutreach$graphicsvg$GraphicSVG$rgba,
						0,
						182,
						255,
						0.6 + (0.4 * $elm$core$Basics$sin((5 * model.aO) - 1))),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h)),
					shape
				])) : shape;
	});
var $author$project$PolygonCreator$titleColour = A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 0, 84, 212);
var $author$project$PolygonCreator$colours = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, 0),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 130, 90)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, 44),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 75, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-75, 41),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$PolygonCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('3. Pick a Colour!')))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(2, 22),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
					_List_fromArray(
						[
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$PolygonCreator$Buttondown(6),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$PolygonCreator$Buttondown(0),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$PolygonCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													G: (m.G < 254) ? (m.G + 1) : 255
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-95, -10),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(-30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 255, 10, 10),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$PolygonCreator$Buttondown(6),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$PolygonCreator$Buttondown(1),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$PolygonCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													G: (m.G > 1) ? (m.G - 1) : 0
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-84, -9),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 180, 140, 140),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$PolygonCreator$Buttondown(6),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$PolygonCreator$Buttondown(2),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$PolygonCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													D: (m.D < 254) ? (m.D + 1) : 255
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-65, -10),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(-30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 10, 255, 10),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$PolygonCreator$Buttondown(6),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$PolygonCreator$Buttondown(3),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$PolygonCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													D: (m.D > 1) ? (m.D - 1) : 0
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-54, -9),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 140, 180, 140),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$PolygonCreator$Buttondown(6),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$PolygonCreator$Buttondown(4),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$PolygonCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													C: (m.C < 254) ? (m.C + 1) : 255
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-35, -10),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(-30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 10, 10, 255),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$PolygonCreator$Buttondown(6),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$PolygonCreator$Buttondown(5),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$PolygonCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													C: (m.C > 1) ? (m.C - 1) : 0
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-24, -9),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 140, 140, 180),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8)))))))
						]))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (ss, y) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(0, 30),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(-40, y),
									A5(
										$author$project$PolygonCreator$time3,
										model,
										ss,
										130,
										10,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(-63, -2.5),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
												$author$project$PolygonCreator$SetColour(ss),
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$filled,
													$MacCASOutreach$graphicsvg$GraphicSVG$black,
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$size,
														9,
														$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
															$MacCASOutreach$graphicsvg$GraphicSVG$text(
																A2($author$project$PolygonCreator$clrString, model, ss))))))))));
						}),
					_List_fromArray(
						[0]),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-10) * x;
						},
						A2($elm$core$List$range, 0, 40))))
			]));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$blue = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 52, 101, 164, 1);
var $author$project$PolygonCreator$flash = function (model) {
	return (model.j === 1) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$scale,
				$elm$core$Basics$abs(
					($elm$core$Basics$sin(model.aO * 2) / 2) + 1.5),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					model.R,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						$MacCASOutreach$graphicsvg$GraphicSVG$circle(2)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-190, 156),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
					$elm$core$Basics$abs(
						$elm$core$Basics$sin(model.aO)) * 0.25,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 12))))
			])) : ((model.j === 2) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$scale,
				$elm$core$Basics$abs(
					($elm$core$Basics$sin(model.aO * 2) / 2) + 1.5),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					model.af,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						$MacCASOutreach$graphicsvg$GraphicSVG$circle(2)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-190, 136),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
					$elm$core$Basics$abs(
						$elm$core$Basics$sin(model.aO)) * 0.25,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 12))))
			])) : ((model.j === 3) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$scale,
				$elm$core$Basics$abs(
					($elm$core$Basics$sin(model.aO * 2) / 2) + 1.5),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					model.ag,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						$MacCASOutreach$graphicsvg$GraphicSVG$circle(2)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-190, 116),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
					$elm$core$Basics$abs(
						$elm$core$Basics$sin(model.aO)) * 0.25,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 12))))
			])) : ((model.j === 4) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$scale,
				$elm$core$Basics$abs(
					($elm$core$Basics$sin(model.aO * 2) / 2) + 1.5),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					model.ah,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						$MacCASOutreach$graphicsvg$GraphicSVG$circle(2)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-190, 96),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
					$elm$core$Basics$abs(
						$elm$core$Basics$sin(model.aO)) * 0.25,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 12))))
			])) : ((model.j === 5) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$scale,
				$elm$core$Basics$abs(
					($elm$core$Basics$sin(model.aO * 2) / 2) + 1.5),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					model.ai,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						$MacCASOutreach$graphicsvg$GraphicSVG$circle(2)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-190, 76),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
					$elm$core$Basics$abs(
						$elm$core$Basics$sin(model.aO)) * 0.25,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 12))))
			])) : ((model.j === 6) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$scale,
				$elm$core$Basics$abs(
					($elm$core$Basics$sin(model.aO * 2) / 2) + 1.5),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					model.aj,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						$MacCASOutreach$graphicsvg$GraphicSVG$circle(2)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-190, 56),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
					$elm$core$Basics$abs(
						$elm$core$Basics$sin(model.aO)) * 0.25,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 12))))
			])) : ((model.j === 7) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$scale,
				$elm$core$Basics$abs(
					($elm$core$Basics$sin(model.aO * 2) / 2) + 1.5),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					model.ak,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						$MacCASOutreach$graphicsvg$GraphicSVG$circle(2)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-190, 36),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
					$elm$core$Basics$abs(
						$elm$core$Basics$sin(model.aO)) * 0.25,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 12))))
			])) : ((model.j === 8) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$scale,
				$elm$core$Basics$abs(
					($elm$core$Basics$sin(model.aO * 2) / 2) + 1.5),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					model.al,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						$MacCASOutreach$graphicsvg$GraphicSVG$circle(2)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-190, 16),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
					$elm$core$Basics$abs(
						$elm$core$Basics$sin(model.aO)) * 0.25,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 12))))
			])) : ((model.j === 9) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$scale,
				$elm$core$Basics$abs(
					($elm$core$Basics$sin(model.aO * 2) / 2) + 1.5),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					model.am,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						$MacCASOutreach$graphicsvg$GraphicSVG$circle(2)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-190, -4),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
					$elm$core$Basics$abs(
						$elm$core$Basics$sin(model.aO)) * 0.25,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 12))))
			])) : ((model.j === 10) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$scale,
				$elm$core$Basics$abs(
					($elm$core$Basics$sin(model.aO * 2) / 2) + 1.5),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					model.F,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$blue,
						$MacCASOutreach$graphicsvg$GraphicSVG$circle(2)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(0, ((-20) * model.z) - 30),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-190, 164),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
						$elm$core$Basics$abs(
							$elm$core$Basics$sin(model.aO)) * 0.25,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$blue,
							A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 12)))))
			])) : A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
		0,
		A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$filled,
			$MacCASOutreach$graphicsvg$GraphicSVG$blue,
			$MacCASOutreach$graphicsvg$GraphicSVG$circle(2))))))))))));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$graphPaper = function (s) {
	return A3(
		$MacCASOutreach$graphicsvg$GraphicSVG$GraphPaper,
		s,
		1,
		A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 135, 206, 250, 1));
};
var $author$project$PolygonCreator$LongPress = function (a) {
	return {$: 15, a: a};
};
var $author$project$PolygonCreator$MovePoint = function (a) {
	return {$: 7, a: a};
};
var $author$project$PolygonCreator$keypad = F2(
	function (location, colour) {
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			location,
			$MacCASOutreach$graphicsvg$GraphicSVG$group(
				_List_fromArray(
					[
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
						$author$project$PolygonCreator$LongPress(4),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
							$author$project$PolygonCreator$LongPress(0),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$PolygonCreator$MovePoint(0),
								A3(
									$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
									$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
									colour,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										colour,
										$MacCASOutreach$graphicsvg$GraphicSVG$polygon(
											_List_fromArray(
												[
													_Utils_Tuple2(0, 0),
													_Utils_Tuple2(80, 0),
													_Utils_Tuple2(40, 10),
													_Utils_Tuple2(0, 0)
												]))))))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
						$author$project$PolygonCreator$LongPress(4),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
							$author$project$PolygonCreator$LongPress(2),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$PolygonCreator$MovePoint(2),
								A3(
									$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
									$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
									colour,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										colour,
										$MacCASOutreach$graphicsvg$GraphicSVG$polygon(
											_List_fromArray(
												[
													_Utils_Tuple2(0, 0),
													_Utils_Tuple2(0, -12),
													_Utils_Tuple2(-10, -6),
													_Utils_Tuple2(0, 0)
												]))))))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
						$author$project$PolygonCreator$LongPress(4),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
							$author$project$PolygonCreator$LongPress(1),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$PolygonCreator$MovePoint(1),
								A3(
									$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
									$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
									colour,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										colour,
										$MacCASOutreach$graphicsvg$GraphicSVG$polygon(
											_List_fromArray(
												[
													_Utils_Tuple2(0, -12),
													_Utils_Tuple2(80, -12),
													_Utils_Tuple2(40, -22),
													_Utils_Tuple2(0, -12)
												]))))))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
						$author$project$PolygonCreator$LongPress(4),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
							$author$project$PolygonCreator$LongPress(3),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$PolygonCreator$MovePoint(3),
								A3(
									$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
									$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
									colour,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										colour,
										$MacCASOutreach$graphicsvg$GraphicSVG$polygon(
											_List_fromArray(
												[
													_Utils_Tuple2(80, -12),
													_Utils_Tuple2(80, 0),
													_Utils_Tuple2(90, -6)
												])))))))
					])));
	});
var $author$project$PolygonCreator$moveTrack = F2(
	function (u, m) {
		if (u) {
			return _Utils_Tuple2(-500, 85);
		} else {
			var _v0 = m.j;
			switch (_v0) {
				case 0:
					return _Utils_Tuple2(-500, 161);
				case 1:
					return _Utils_Tuple2(-235, 161);
				case 2:
					return _Utils_Tuple2(-235, 141);
				case 3:
					return _Utils_Tuple2(-235, 121);
				case 4:
					return _Utils_Tuple2(-235, 101);
				case 5:
					return _Utils_Tuple2(-235, 81);
				case 6:
					return _Utils_Tuple2(-235, 61);
				case 7:
					return _Utils_Tuple2(-235, 41);
				case 8:
					return _Utils_Tuple2(-235, 21);
				case 9:
					return _Utils_Tuple2(-235, 1);
				default:
					return _Utils_Tuple2(-235, ((-20) * m.z) + 140);
			}
		}
	});
var $author$project$PolygonCreator$CheckPoint = function (a) {
	return {$: 10, a: a};
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$PolygonCreator$polyCode = function (m) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-19.5, -90),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 160, 215)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, 19),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-75, 16),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$PolygonCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('1. Polygon Points')))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-80, 0),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
					_Utils_ap(
						_List_fromArray(
							[
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-10, 0),
								$author$project$PolygonCreator$code('polygon')),
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-2, -12),
								$author$project$PolygonCreator$code('[')),
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(4, -12),
								$author$project$PolygonCreator$code('(')),
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(10, -13),
								$author$project$PolygonCreator$code(
									$elm$core$String$fromFloat(m.R.a))),
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(40, -13),
								$author$project$PolygonCreator$code(',')),
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(49, -13),
								$author$project$PolygonCreator$code(
									$elm$core$String$fromFloat(m.R.b))),
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(78, -12),
								$author$project$PolygonCreator$code(')')),
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$PolygonCreator$CheckPoint(1),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
									0,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(45, -10),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$filled,
											$MacCASOutreach$graphicsvg$GraphicSVG$black,
											A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 10)))))
							]),
						_Utils_ap(
							A3(
								$elm$core$List$map2,
								F2(
									function (str, y) {
										return A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											$author$project$PolygonCreator$CheckPoint(
												$elm$core$Basics$round(y / (-10))),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$move,
												_Utils_Tuple2(0, y * 2),
												str));
									}),
								A2(
									$elm$core$List$map,
									function (_v0) {
										var flag = _v0.a;
										var t = _v0.b;
										var w = _v0.c;
										return flag ? (w ? A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$filled,
											$MacCASOutreach$graphicsvg$GraphicSVG$white,
											A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0, 0)) : $MacCASOutreach$graphicsvg$GraphicSVG$group(
											_List_fromArray(
												[
													A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$move,
													_Utils_Tuple2(-2, -2),
													$author$project$PolygonCreator$code(',')),
													A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$move,
													_Utils_Tuple2(4, -2),
													$author$project$PolygonCreator$code('(')),
													A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$move,
													_Utils_Tuple2(10, -3),
													$author$project$PolygonCreator$code(
														$elm$core$String$fromFloat(t.a))),
													A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$move,
													_Utils_Tuple2(40, -3),
													$author$project$PolygonCreator$code(',')),
													A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$move,
													_Utils_Tuple2(49, -3),
													$author$project$PolygonCreator$code(
														$elm$core$String$fromFloat(t.b))),
													A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$move,
													_Utils_Tuple2(78, -2),
													$author$project$PolygonCreator$code(')')),
													A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
													0,
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$move,
														_Utils_Tuple2(45, 0),
														A2(
															$MacCASOutreach$graphicsvg$GraphicSVG$filled,
															$MacCASOutreach$graphicsvg$GraphicSVG$black,
															A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 10))))
												]))) : A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$filled,
											$MacCASOutreach$graphicsvg$GraphicSVG$white,
											A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0, 0));
									},
									_List_fromArray(
										[
											_Utils_Tuple3(
											A2(
												$elm$core$Maybe$withDefault,
												false,
												A2(
													$elm$core$Array$get,
													1,
													$elm$core$Array$fromList(m.K))),
											m.af,
											false),
											_Utils_Tuple3(
											A2(
												$elm$core$Maybe$withDefault,
												false,
												A2(
													$elm$core$Array$get,
													2,
													$elm$core$Array$fromList(m.K))),
											m.ag,
											false),
											_Utils_Tuple3(
											A2(
												$elm$core$Maybe$withDefault,
												false,
												A2(
													$elm$core$Array$get,
													3,
													$elm$core$Array$fromList(m.K))),
											m.ah,
											false),
											_Utils_Tuple3(
											A2(
												$elm$core$Maybe$withDefault,
												false,
												A2(
													$elm$core$Array$get,
													4,
													$elm$core$Array$fromList(m.K))),
											m.ai,
											false),
											_Utils_Tuple3(
											A2(
												$elm$core$Maybe$withDefault,
												false,
												A2(
													$elm$core$Array$get,
													5,
													$elm$core$Array$fromList(m.K))),
											m.aj,
											false),
											_Utils_Tuple3(
											A2(
												$elm$core$Maybe$withDefault,
												false,
												A2(
													$elm$core$Array$get,
													6,
													$elm$core$Array$fromList(m.K))),
											m.ak,
											false),
											_Utils_Tuple3(
											A2(
												$elm$core$Maybe$withDefault,
												false,
												A2(
													$elm$core$Array$get,
													7,
													$elm$core$Array$fromList(m.K))),
											m.al,
											false),
											_Utils_Tuple3(
											A2(
												$elm$core$Maybe$withDefault,
												false,
												A2(
													$elm$core$Array$get,
													8,
													$elm$core$Array$fromList(m.K))),
											m.am,
											false)
										])),
								_List_fromArray(
									[-15, -25, -35, -45, -55, -65, -75, -85, -95, -105])),
							_List_fromArray(
								[
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$PolygonCreator$CheckPoint(10),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(0, ((-20) * m.z) - 30),
										$MacCASOutreach$graphicsvg$GraphicSVG$group(
											_List_fromArray(
												[
													A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$move,
													_Utils_Tuple2(-2, -2),
													$author$project$PolygonCreator$code(',')),
													A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$move,
													_Utils_Tuple2(4, -2),
													$author$project$PolygonCreator$code('(')),
													A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$move,
													_Utils_Tuple2(10, -3),
													$author$project$PolygonCreator$code(
														$elm$core$String$fromFloat(m.F.a))),
													A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$move,
													_Utils_Tuple2(40, -3),
													$author$project$PolygonCreator$code(',')),
													A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$move,
													_Utils_Tuple2(49, -3),
													$author$project$PolygonCreator$code(
														$elm$core$String$fromFloat(m.F.b))),
													A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$move,
													_Utils_Tuple2(78, -2),
													$author$project$PolygonCreator$code(')')),
													A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$move,
													_Utils_Tuple2(84, -2),
													$author$project$PolygonCreator$code(']')),
													A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
													0,
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$move,
														_Utils_Tuple2(45, 0),
														A2(
															$MacCASOutreach$graphicsvg$GraphicSVG$filled,
															$MacCASOutreach$graphicsvg$GraphicSVG$black,
															A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 10))))
												]))))
								])))))
			]));
};
var $author$project$PolygonCreator$colourFun = function (m) {
	var _v0 = m.bj;
	return A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, m.G, m.D, m.C);
};
var $author$project$PolygonCreator$lineStyleFun = function (m) {
	var _v0 = m.br;
	switch (_v0) {
		case 0:
			return $MacCASOutreach$graphicsvg$GraphicSVG$solid(m.I);
		case 1:
			return $MacCASOutreach$graphicsvg$GraphicSVG$dotted(m.I);
		case 2:
			return $MacCASOutreach$graphicsvg$GraphicSVG$dashed(m.I);
		case 3:
			return $MacCASOutreach$graphicsvg$GraphicSVG$longdash(m.I);
		default:
			return $MacCASOutreach$graphicsvg$GraphicSVG$dotdash(m.I);
	}
};
var $author$project$PolygonCreator$shapeFun = function (m) {
	var _v0 = m.aV;
	if (!_v0) {
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$filled,
			$author$project$PolygonCreator$colourFun(m),
			$MacCASOutreach$graphicsvg$GraphicSVG$polygon(
				_Utils_ap(
					m.c,
					_List_fromArray(
						[m.F]))));
	} else {
		return (m.aZ ? $MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent(m.ad) : function (x) {
			return x;
		})(
			(m.a1 ? $MacCASOutreach$graphicsvg$GraphicSVG$scaleY(m._) : function (x) {
				return x;
			})(
				(m.a0 ? $MacCASOutreach$graphicsvg$GraphicSVG$scaleX(m._) : function (x) {
					return x;
				})(
					(m.a$ ? $MacCASOutreach$graphicsvg$GraphicSVG$scale(m._) : function (x) {
						return x;
					})(
						(m.a_ ? $MacCASOutreach$graphicsvg$GraphicSVG$rotate(
							$elm$core$Basics$degrees(m.aI)) : function (x) {
							return x;
						})(
							(m.b5 ? $MacCASOutreach$graphicsvg$GraphicSVG$move(
								_Utils_Tuple2(m.cq, m.cr)) : function (x) {
								return x;
							})(
								A3(
									$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
									$author$project$PolygonCreator$lineStyleFun(m),
									$author$project$PolygonCreator$colourFun(m),
									$MacCASOutreach$graphicsvg$GraphicSVG$polygon(
										_Utils_ap(
											m.c,
											_List_fromArray(
												[m.F]))))))))));
	}
};
var $author$project$PolygonCreator$Draw = function (a) {
	return {$: 1, a: a};
};
var $author$project$PolygonCreator$LStyle = {$: 2};
var $author$project$PolygonCreator$Outlined = 1;
var $author$project$PolygonCreator$stampString = F2(
	function (m, stamp) {
		if (!stamp) {
			return 'filled ';
		} else {
			return 'outlined ';
		}
	});
var $author$project$PolygonCreator$styleString = function (m) {
	return '(' + (function () {
		var _v0 = m.br;
		switch (_v0) {
			case 0:
				return 'solid ';
			case 1:
				return 'dotted ';
			case 2:
				return 'dashed ';
			case 3:
				return 'longdash ';
			default:
				return 'dotdash ';
		}
	}() + ($elm$core$String$fromFloat(m.I) + ')'));
};
var $author$project$PolygonCreator$time2 = F5(
	function (model, ss, w, h, shape) {
		return _Utils_eq(ss, model.aV) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4(
						$MacCASOutreach$graphicsvg$GraphicSVG$rgba,
						0,
						182,
						255,
						0.6 + (0.4 * $elm$core$Basics$sin((5 * model.aO) - 0.5))),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h)),
					shape
				])) : shape;
	});
var $author$project$PolygonCreator$stamps = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, -1),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 130, 30)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, 14),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 95, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-85, 11),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$PolygonCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('2. Fill it or Outline it!')))))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (ss, y) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-40, y),
								A5(
									$author$project$PolygonCreator$time2,
									model,
									ss,
									130,
									10,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-63, -2.5),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											$author$project$PolygonCreator$Draw(ss),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$black,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$size,
													9,
													$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
														$MacCASOutreach$graphicsvg$GraphicSVG$text(
															A2($author$project$PolygonCreator$stampString, model, ss)))))))));
						}),
					_List_fromArray(
						[0, 1]),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-10) * x;
						},
						A2($elm$core$List$range, 0, 20)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
				$author$project$PolygonCreator$LStyle,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-43, -12.5),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$black,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$size,
							10,
							$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
								$MacCASOutreach$graphicsvg$GraphicSVG$text(
									$author$project$PolygonCreator$styleString(model)))))))
			]));
};
var $author$project$PolygonCreator$tweaks = $MacCASOutreach$graphicsvg$GraphicSVG$group(
	_List_fromArray(
		[
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-35, 1),
			A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
				$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
				$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 140, 25)))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-60, 14),
			A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
				$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
				$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$white,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 55, 12)))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-85, 11),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$author$project$PolygonCreator$titleColour,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					10,
					$MacCASOutreach$graphicsvg$GraphicSVG$italic(
						$MacCASOutreach$graphicsvg$GraphicSVG$serif(
							$MacCASOutreach$graphicsvg$GraphicSVG$text('4. Tweak it!')))))),
			$MacCASOutreach$graphicsvg$GraphicSVG$group(
			A3(
				$elm$core$List$map2,
				F2(
					function (_v0, _v1) {
						var str = _v0.a;
						var msg = _v0.b;
						var x = _v1.a;
						var y = _v1.b;
						return A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2((-68) + x, (-2.5) + y),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								msg,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$black,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$size,
										10,
										$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
											$MacCASOutreach$graphicsvg$GraphicSVG$text(str))))));
					}),
				_List_fromArray(
					[
						_Utils_Tuple2(
						'thicker',
						$author$project$PolygonCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										I: (m.I < 10) ? (m.I + 0.5) : 10
									});
							})),
						_Utils_Tuple2(
						'thinner',
						$author$project$PolygonCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										I: (m.I > 0.5) ? (m.I - 0.5) : 0.5
									});
							}))
					]),
				$elm$core$List$concat(
					A2(
						$elm$core$List$map,
						function (idx) {
							return _List_fromArray(
								[
									_Utils_Tuple2(-30, (-10) * idx),
									_Utils_Tuple2(40, (-10) * idx)
								]);
						},
						A2($elm$core$List$range, 0, 20)))))
		]));
var $elm$core$List$map3 = _List_map3;
var $author$project$PolygonCreator$vertices = function (model) {
	return model.bA ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A4(
					$elm$core$List$map3,
					F3(
						function (ss, y, n) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
								_Utils_eq(n, model.j) ? 0 : 1,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									ss,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$red,
										$MacCASOutreach$graphicsvg$GraphicSVG$circle(2))));
						}),
					_Utils_ap(
						model.c,
						_List_fromArray(
							[model.F])),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-10) * x;
						},
						A2($elm$core$List$range, 0, 20)),
					_List_fromArray(
						[1, 2, 3, 4, 5, 6, 7, 8, 9, 10])))
			])) : A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
		0,
		A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$filled,
			$MacCASOutreach$graphicsvg$GraphicSVG$white,
			A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 20, 20)));
};
var $author$project$PolygonCreator$MakeTransparent = 5;
var $author$project$PolygonCreator$Rotate = 1;
var $author$project$PolygonCreator$Scale = 2;
var $author$project$PolygonCreator$ScaleX = 3;
var $author$project$PolygonCreator$ScaleY = 4;
var $author$project$PolygonCreator$pullsText1 = function (m) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				$author$project$ShapeCreateAssets$copiable(
				'polygon [' + (A2(
					$elm$core$String$join,
					',',
					A2(
						$elm$core$List$map,
						$author$project$ShapeCreateAssets$ptCode,
						_Utils_ap(
							m.c,
							_List_fromArray(
								[m.F])))) + ']'))
			]));
};
var $author$project$PolygonCreator$transformString = F2(
	function (m, t) {
		switch (t) {
			case 0:
				return '|> move (' + ($elm$core$String$fromFloat(m.cv) + (',' + ($elm$core$String$fromFloat(m.bg) + ')')));
			case 1:
				return '|> rotate (degrees ' + ($elm$core$String$fromFloat(m.aI) + ')');
			case 2:
				return '|> scale ' + $elm$core$String$fromFloat(m._);
			case 3:
				return '|> scaleX ' + $elm$core$String$fromFloat(m.aB);
			case 4:
				return '|> scaleY ' + $elm$core$String$fromFloat(m.aC);
			default:
				return '|> makeTransparent ' + $elm$core$String$fromFloat(m.ad);
		}
	});
var $author$project$PolygonCreator$yourCode1 = function (m) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$move,
		_Utils_Tuple2(0, -40),
		$MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(0, -30),
					A3(
						$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
						$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
						$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
							A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 490, 85)))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-145, 14),
					A3(
						$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
						$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
						$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$white,
							A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 110, 12)))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-190, 11),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$author$project$PolygonCreator$titleColour,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$size,
							10,
							$MacCASOutreach$graphicsvg$GraphicSVG$italic(
								$MacCASOutreach$graphicsvg$GraphicSVG$serif(
									$MacCASOutreach$graphicsvg$GraphicSVG$text('5. Your (copiable) code! Use cmd/ctrl-A, cmd/ctrl-C. ')))))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-240, 0),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$scale,
						0.33,
						$author$project$PolygonCreator$pullsText1(m))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-220, 0),
					$MacCASOutreach$graphicsvg$GraphicSVG$group(
						_Utils_ap(
							_List_fromArray(
								[
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(0, -5),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$scale,
										0.33,
										$author$project$ShapeCreateAssets$copiable(
											'      |> ' + (A2($author$project$PolygonCreator$stampString, m, m.aV) + (((m.aV === 1) ? ($author$project$PolygonCreator$styleString(m) + ' ') : '') + A2($author$project$PolygonCreator$clrString, m, m.bj))))))
								]),
							A3(
								$elm$core$List$map2,
								F2(
									function (str, y) {
										return A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(0, y),
											$author$project$PolygonCreator$code(str));
									}),
								$elm$core$List$concat(
									A2(
										$elm$core$List$map,
										function (_v0) {
											var flag = _v0.a;
											var t = _v0.b;
											return flag ? _List_fromArray(
												[
													' ' + A2($author$project$PolygonCreator$transformString, m, t)
												]) : _List_Nil;
										},
										_List_fromArray(
											[
												_Utils_Tuple2(m.a$, 2),
												_Utils_Tuple2(m.a0, 3),
												_Utils_Tuple2(m.a1, 4),
												_Utils_Tuple2(m.a_, 1),
												_Utils_Tuple2(m.aZ, 5)
											]))),
								_List_fromArray(
									[-20, -30, -40, -50, -60, -70, -80])))))
				])));
};
var $author$project$PolygonCreator$view = function (model) {
	return _List_fromArray(
		[
			$MacCASOutreach$graphicsvg$GraphicSVG$graphPaper(10),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$filled,
			$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
			A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 512, 0.5)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$filled,
			$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
			A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 512)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(0, 100),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 4, 0.5))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(3, 100),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(0,100)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(0, -100),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 4, 0.5))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(3, -100),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(0,-100)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-100, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-100, 3),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(-100,0)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(100, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(100, 3),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(100,0)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-200, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-200, 3),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(-200,0)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(200, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(200, 3),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(200,0)')))),
			$author$project$PolygonCreator$shapeFun(model),
			$author$project$PolygonCreator$vertices(model),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-45, 165),
			$author$project$PolygonCreator$code('|>')),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(80, 169),
			$author$project$PolygonCreator$stamps(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(230, 139),
			$author$project$PolygonCreator$colours(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-150, -70),
			$author$project$PolygonCreator$tweaks),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
			model.by * $elm$core$Basics$abs(
				$elm$core$Basics$sin(3 * model.aO)),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-190, 135),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$red,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 12)))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$scale,
			$elm$core$Basics$abs(
				$elm$core$Basics$sin(model.aO)),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-180, 100),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
					model.by * $elm$core$Basics$abs(
						$elm$core$Basics$sin(model.aO)),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(1, 56),
						$author$project$PolygonCreator$clickhere(model))))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(0, 280),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-155, -115),
				$author$project$PolygonCreator$polyCode(model))),
			$author$project$PolygonCreator$flash(model),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(5, 0),
			A2(
				$author$project$PolygonCreator$keypad,
				A2($author$project$PolygonCreator$moveTrack, model.bn, model),
				A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 49, 49, 129, 0.2))),
			$author$project$PolygonCreator$button(model),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(0, -120),
			$author$project$PolygonCreator$yourCode1(model))
		]);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$brown = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 193, 125, 17, 1);
var $author$project$ShapeCreator$Black = 0;
var $author$project$ShapeCreator$Blank = 1;
var $author$project$ShapeCreator$Blue = 2;
var $author$project$ShapeCreator$BlueDown = 3;
var $author$project$ShapeCreator$BlueUp = 2;
var $author$project$ShapeCreator$Brown = 3;
var $author$project$ShapeCreator$ButtonDown = function (a) {
	return {$: 9, a: a};
};
var $author$project$ShapeCreator$Charcoal = 4;
var $author$project$ShapeCreator$DarkBlue = 5;
var $author$project$ShapeCreator$DarkBrown = 6;
var $author$project$ShapeCreator$DarkCharcoal = 7;
var $author$project$ShapeCreator$DarkGray = 8;
var $author$project$ShapeCreator$DarkGreen = 9;
var $author$project$ShapeCreator$DarkOrange = 11;
var $author$project$ShapeCreator$DarkPurple = 12;
var $author$project$ShapeCreator$DarkRed = 13;
var $author$project$ShapeCreator$DarkYellow = 14;
var $author$project$ShapeCreator$Gray = 15;
var $author$project$ShapeCreator$Green = 16;
var $author$project$ShapeCreator$GreenDown = 5;
var $author$project$ShapeCreator$GreenUp = 4;
var $author$project$ShapeCreator$HotPink = 18;
var $author$project$ShapeCreator$LightBlue = 19;
var $author$project$ShapeCreator$LightBrown = 20;
var $author$project$ShapeCreator$LightCharcoal = 21;
var $author$project$ShapeCreator$LightGray = 22;
var $author$project$ShapeCreator$LightGreen = 23;
var $author$project$ShapeCreator$LightOrange = 25;
var $author$project$ShapeCreator$LightPurple = 26;
var $author$project$ShapeCreator$LightRed = 27;
var $author$project$ShapeCreator$LightYellow = 28;
var $author$project$ShapeCreator$Orange = 29;
var $author$project$ShapeCreator$Pink = 30;
var $author$project$ShapeCreator$Purple = 31;
var $author$project$ShapeCreator$Red = 32;
var $author$project$ShapeCreator$RedDown = 1;
var $author$project$ShapeCreator$RedUp = 0;
var $author$project$ShapeCreator$SetColour = function (a) {
	return {$: 5, a: a};
};
var $author$project$ShapeCreator$TransM = function (a) {
	return {$: 7, a: a};
};
var $author$project$ShapeCreator$White = 33;
var $author$project$ShapeCreator$Yellow = 34;
var $author$project$ShapeCreator$clrString = F2(
	function (m, clr) {
		switch (clr) {
			case 0:
				return 'black';
			case 1:
				return 'blank';
			case 2:
				return 'blue';
			case 3:
				return 'brown';
			case 4:
				return 'charcoal';
			case 5:
				return 'darkBlue';
			case 6:
				return 'darkBrown';
			case 7:
				return 'darkCharcoal';
			case 8:
				return 'darkGray';
			case 9:
				return 'darkGreen';
			case 10:
				return 'darkGrey';
			case 11:
				return 'darkOrange';
			case 12:
				return 'darkPurple';
			case 13:
				return 'darkRed';
			case 14:
				return 'darkYellow';
			case 15:
				return 'gray';
			case 16:
				return 'green';
			case 17:
				return 'grey';
			case 18:
				return 'hotPink';
			case 19:
				return 'lightBlue';
			case 20:
				return 'lightBrown';
			case 21:
				return 'lightCharcoal';
			case 22:
				return 'lightGray';
			case 23:
				return 'lightGreen';
			case 24:
				return 'lightGrey';
			case 25:
				return 'lightOrange';
			case 26:
				return 'lightPurple';
			case 27:
				return 'lightRed';
			case 28:
				return 'lightYellow';
			case 29:
				return 'orange';
			case 30:
				return 'pink';
			case 31:
				return 'purple';
			case 32:
				return 'red';
			case 33:
				return 'white';
			case 34:
				return 'yellow';
			default:
				return '(rgb ' + ($elm$core$String$fromFloat(m.G) + (' ' + ($elm$core$String$fromFloat(m.D) + (' ' + ($elm$core$String$fromFloat(m.C) + ')')))));
		}
	});
var $author$project$ShapeCreator$code = function (str) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$filled,
		$MacCASOutreach$graphicsvg$GraphicSVG$black,
		A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$size,
			10,
			$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
				$MacCASOutreach$graphicsvg$GraphicSVG$text(str))));
};
var $author$project$ShapeCreator$colourAmount = 3;
var $author$project$ShapeCreateAssets$All = 6;
var $author$project$ShapeCreateAssets$BlueAndGreen = 5;
var $author$project$ShapeCreateAssets$BrBlue = 1;
var $author$project$ShapeCreateAssets$BrGreen = 2;
var $author$project$ShapeCreateAssets$BrRed = 0;
var $author$project$ShapeCreateAssets$RedAndBlue = 3;
var $author$project$ShapeCreateAssets$RedAndGreen = 4;
var $author$project$ShapeCreateAssets$getBrightest = F3(
	function (r, g, b) {
		return (_Utils_eq(r, g) && _Utils_eq(g, b)) ? 6 : ((_Utils_cmp(r, b) > 0) ? ((_Utils_cmp(r, g) > 0) ? 0 : (_Utils_eq(r, g) ? 4 : 2)) : (_Utils_eq(r, b) ? ((_Utils_cmp(r, g) > 0) ? 3 : 2) : ((_Utils_cmp(b, g) > 0) ? 1 : (_Utils_eq(b, g) ? 5 : 2))));
	});
var $author$project$ShapeCreator$time3 = F5(
	function (model, ss, w, h, shape) {
		return _Utils_eq(ss, model.bj) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4(
						$MacCASOutreach$graphicsvg$GraphicSVG$rgba,
						255,
						137,
						5,
						0.6 + (0.4 * $elm$core$Basics$sin((5 * model.aO) - 1))),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h)),
					shape
				])) : shape;
	});
var $author$project$ShapeCreator$titleColour = A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 255, 112, 0);
var $author$project$ShapeCreator$colours = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, -170),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 130, 370)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, 14),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 75, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-75, 11),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$ShapeCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('3. Pick a Colour!')))))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (ss, y) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-40, y),
								A5(
									$author$project$ShapeCreator$time3,
									model,
									ss,
									130,
									10,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-63, -2.5),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											$author$project$ShapeCreator$SetColour(ss),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$black,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$size,
													10,
													$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
														$MacCASOutreach$graphicsvg$GraphicSVG$text(
															A2($author$project$ShapeCreator$clrString, model, ss)))))))));
						}),
					_List_fromArray(
						[0, 33, 1, 2, 5, 19, 3, 6, 20, 4, 7, 21, 15, 8, 22, 16, 9, 23, 29, 11, 25, 30, 18, 31, 12, 26, 34, 14, 28, 32, 13, 27]),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-10) * x;
						},
						A2($elm$core$List$range, 0, 40)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, -338),
				A5(
					$author$project$ShapeCreator$time3,
					model,
					35,
					130,
					46,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
						$author$project$ShapeCreator$SetColour(35),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 1, 1, 1, 0),
							A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 130, 46))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(0, -315),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
					_List_fromArray(
						[
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$ShapeCreator$ButtonDown(6),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$ShapeCreator$ButtonDown(0),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$ShapeCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													G: (m.G < 254) ? (m.G + 1) : 255
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-95, -10),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(-30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 255, 10, 10),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$ShapeCreator$ButtonDown(6),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$ShapeCreator$ButtonDown(1),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$ShapeCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													G: (m.G > 1) ? (m.G - 1) : 0
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-84, -9),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 180, 140, 140),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$ShapeCreator$ButtonDown(6),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$ShapeCreator$ButtonDown(4),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$ShapeCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													D: (m.D < 254) ? (m.D + 1) : 255
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-65, -10),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(-30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 10, 255, 10),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$ShapeCreator$ButtonDown(6),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$ShapeCreator$ButtonDown(5),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$ShapeCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													D: (m.D > 1) ? (m.D - 1) : 0
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-54, -9),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 140, 180, 140),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$ShapeCreator$ButtonDown(6),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$ShapeCreator$ButtonDown(2),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$ShapeCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													C: (m.C < 254) ? (m.C + 1) : 255
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-35, -10),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(-30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 10, 10, 255),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$ShapeCreator$ButtonDown(6),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$ShapeCreator$ButtonDown(3),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$ShapeCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													C: (m.C > 1) ? (m.C - 1) : 0
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-24, -9),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 140, 140, 180),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(-90, -25),
							$author$project$ShapeCreator$code('lighter')),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$ShapeCreator$TransM(
								function (m) {
									return _Utils_update(
										m,
										{
											C: A3($elm$core$Basics$clamp, 0, 255, m.C + 5),
											D: A3($elm$core$Basics$clamp, 0, 255, m.D + 5),
											G: A3($elm$core$Basics$clamp, 0, 255, m.G + 5)
										});
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-75, -27),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$blank,
									A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 32, 10)))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(-45, -25),
							$author$project$ShapeCreator$code('darker')),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$ShapeCreator$TransM(
								function (m) {
									return _Utils_update(
										m,
										{
											C: A3($elm$core$Basics$clamp, 0, 255, m.C - 5),
											D: A3($elm$core$Basics$clamp, 0, 255, m.D - 5),
											G: A3($elm$core$Basics$clamp, 0, 255, m.G - 5)
										});
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-30, -27),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$blank,
									A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 32, 10)))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(-100, -40),
							$author$project$ShapeCreator$code('colourful')),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$ShapeCreator$TransM(
								function (m) {
									var _v0 = A3($author$project$ShapeCreateAssets$getBrightest, model.G, model.D, model.C);
									switch (_v0) {
										case 6:
											return m;
										case 0:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - $author$project$ShapeCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - $author$project$ShapeCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + ($author$project$ShapeCreator$colourAmount * 2))
												});
										case 2:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - $author$project$ShapeCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + ($author$project$ShapeCreator$colourAmount * 2)),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - $author$project$ShapeCreator$colourAmount)
												});
										case 1:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + ($author$project$ShapeCreator$colourAmount * 2)),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - $author$project$ShapeCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - $author$project$ShapeCreator$colourAmount)
												});
										case 4:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - ($author$project$ShapeCreator$colourAmount * 2)),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + $author$project$ShapeCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + $author$project$ShapeCreator$colourAmount)
												});
										case 3:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + $author$project$ShapeCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - ($author$project$ShapeCreator$colourAmount * 2)),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + $author$project$ShapeCreator$colourAmount)
												});
										default:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + $author$project$ShapeCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + $author$project$ShapeCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - ($author$project$ShapeCreator$colourAmount * 2))
												});
									}
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-80, -47),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$blank,
									A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 40, 10)))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(-44, -40),
							$author$project$ShapeCreator$code('colourless')),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$ShapeCreator$TransM(
								function (m) {
									var _v1 = A3($author$project$ShapeCreateAssets$getBrightest, model.G, model.D, model.C);
									switch (_v1) {
										case 6:
											return m;
										case 0:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + $author$project$ShapeCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + $author$project$ShapeCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - ($author$project$ShapeCreator$colourAmount * 2))
												});
										case 2:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + $author$project$ShapeCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - ($author$project$ShapeCreator$colourAmount * 2)),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + $author$project$ShapeCreator$colourAmount)
												});
										case 1:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - ($author$project$ShapeCreator$colourAmount * 2)),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + $author$project$ShapeCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + $author$project$ShapeCreator$colourAmount)
												});
										case 4:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + ($author$project$ShapeCreator$colourAmount * 2)),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - $author$project$ShapeCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - $author$project$ShapeCreator$colourAmount)
												});
										case 3:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - $author$project$ShapeCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + ($author$project$ShapeCreator$colourAmount * 2)),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - $author$project$ShapeCreator$colourAmount)
												});
										default:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - $author$project$ShapeCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - $author$project$ShapeCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + ($author$project$ShapeCreator$colourAmount * 2))
												});
									}
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-23, -47),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$blank,
									A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 44, 10))))
						])))
			]));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$charcoal = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 85, 87, 83, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$darkBrown = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 143, 89, 2, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$darkCharcoal = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 46, 52, 54, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$darkGray = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 186, 189, 182, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$darkGrey = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 186, 189, 182, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$darkOrange = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 206, 92, 0, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$darkPurple = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 92, 53, 102, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$darkYellow = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 196, 160, 0, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$gray = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 211, 215, 207, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$hotPink = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 255, 0, 66, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$lightBlue = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 114, 159, 207, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$lightBrown = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 233, 185, 110, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$lightCharcoal = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 136, 138, 133, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$lightGray = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 238, 238, 236, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$lightGreen = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 138, 226, 52, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$lightOrange = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 252, 175, 62, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 173, 127, 168, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$lightRed = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 239, 41, 41, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$lightYellow = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 255, 233, 79, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$pink = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 255, 105, 180, 1);
var $MacCASOutreach$graphicsvg$GraphicSVG$yellow = A4($MacCASOutreach$graphicsvg$GraphicSVG$RGBA, 237, 212, 0, 1);
var $author$project$ShapeCreator$colourFun = function (m) {
	var _v0 = m.bj;
	switch (_v0) {
		case 0:
			return $MacCASOutreach$graphicsvg$GraphicSVG$black;
		case 1:
			return $MacCASOutreach$graphicsvg$GraphicSVG$blank;
		case 2:
			return $MacCASOutreach$graphicsvg$GraphicSVG$blue;
		case 3:
			return $MacCASOutreach$graphicsvg$GraphicSVG$brown;
		case 4:
			return $MacCASOutreach$graphicsvg$GraphicSVG$charcoal;
		case 5:
			return $MacCASOutreach$graphicsvg$GraphicSVG$darkBlue;
		case 6:
			return $MacCASOutreach$graphicsvg$GraphicSVG$darkBrown;
		case 7:
			return $MacCASOutreach$graphicsvg$GraphicSVG$darkCharcoal;
		case 8:
			return $MacCASOutreach$graphicsvg$GraphicSVG$darkGray;
		case 9:
			return $MacCASOutreach$graphicsvg$GraphicSVG$darkGreen;
		case 10:
			return $MacCASOutreach$graphicsvg$GraphicSVG$darkGrey;
		case 11:
			return $MacCASOutreach$graphicsvg$GraphicSVG$darkOrange;
		case 12:
			return $MacCASOutreach$graphicsvg$GraphicSVG$darkPurple;
		case 13:
			return $MacCASOutreach$graphicsvg$GraphicSVG$darkRed;
		case 14:
			return $MacCASOutreach$graphicsvg$GraphicSVG$darkYellow;
		case 15:
			return $MacCASOutreach$graphicsvg$GraphicSVG$gray;
		case 16:
			return $MacCASOutreach$graphicsvg$GraphicSVG$green;
		case 17:
			return $MacCASOutreach$graphicsvg$GraphicSVG$grey;
		case 18:
			return $MacCASOutreach$graphicsvg$GraphicSVG$hotPink;
		case 19:
			return $MacCASOutreach$graphicsvg$GraphicSVG$lightBlue;
		case 20:
			return $MacCASOutreach$graphicsvg$GraphicSVG$lightBrown;
		case 21:
			return $MacCASOutreach$graphicsvg$GraphicSVG$lightCharcoal;
		case 22:
			return $MacCASOutreach$graphicsvg$GraphicSVG$lightGray;
		case 23:
			return $MacCASOutreach$graphicsvg$GraphicSVG$lightGreen;
		case 24:
			return $MacCASOutreach$graphicsvg$GraphicSVG$lightGrey;
		case 25:
			return $MacCASOutreach$graphicsvg$GraphicSVG$lightOrange;
		case 26:
			return $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple;
		case 27:
			return $MacCASOutreach$graphicsvg$GraphicSVG$lightRed;
		case 28:
			return $MacCASOutreach$graphicsvg$GraphicSVG$lightYellow;
		case 29:
			return $MacCASOutreach$graphicsvg$GraphicSVG$orange;
		case 30:
			return $MacCASOutreach$graphicsvg$GraphicSVG$pink;
		case 31:
			return $MacCASOutreach$graphicsvg$GraphicSVG$purple;
		case 32:
			return $MacCASOutreach$graphicsvg$GraphicSVG$red;
		case 33:
			return $MacCASOutreach$graphicsvg$GraphicSVG$white;
		case 34:
			return $MacCASOutreach$graphicsvg$GraphicSVG$yellow;
		default:
			return A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, m.G, m.D, m.C);
	}
};
var $author$project$ShapeCreator$lineStyleFun = function (m) {
	var _v0 = m.br;
	switch (_v0) {
		case 0:
			return $MacCASOutreach$graphicsvg$GraphicSVG$solid(m.I);
		case 1:
			return $MacCASOutreach$graphicsvg$GraphicSVG$dotted(m.I);
		case 2:
			return $MacCASOutreach$graphicsvg$GraphicSVG$dashed(m.I);
		case 3:
			return $MacCASOutreach$graphicsvg$GraphicSVG$longdash(m.I);
		default:
			return $MacCASOutreach$graphicsvg$GraphicSVG$dotdash(m.I);
	}
};
var $MacCASOutreach$graphicsvg$GraphicSVG$openPolygon = function (ptList) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$Path(ptList);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$Oval = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$oval = F2(
	function (w, h) {
		return A2($MacCASOutreach$graphicsvg$GraphicSVG$Oval, w, h);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$RoundRect = F3(
	function (a, b, c) {
		return {$: 2, a: a, b: b, c: c};
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$roundedRect = F3(
	function (w, h, r) {
		return A3($MacCASOutreach$graphicsvg$GraphicSVG$RoundRect, w, h, r);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$square = function (r) {
	return A2($MacCASOutreach$graphicsvg$GraphicSVG$Rect, r, r);
};
var $MacCASOutreach$graphicsvg$GraphicSVG$wedgeHelper = F2(
	function (r, cn) {
		var angle = $elm$core$Basics$turns(0.5 * cn);
		return _Utils_Tuple2(
			r * $elm$core$Basics$cos(angle),
			r * $elm$core$Basics$sin(angle));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$wedge = F2(
	function (r, frac) {
		var n = ((frac * 360) / 10) + 5;
		var ni = $elm$core$Basics$round(n);
		var dlta = frac / ni;
		return $MacCASOutreach$graphicsvg$GraphicSVG$Polygon(
			(frac > 0) ? _Utils_ap(
				_List_fromArray(
					[
						_Utils_Tuple2(0, 0),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$wedgeHelper, r, -frac)
					]),
				_Utils_ap(
					A2(
						$elm$core$List$map,
						A2(
							$elm$core$Basics$composeL,
							A2(
								$elm$core$Basics$composeL,
								$MacCASOutreach$graphicsvg$GraphicSVG$wedgeHelper(r),
								$elm$core$Basics$mul(dlta)),
							$elm$core$Basics$toFloat),
						A2($elm$core$List$range, -ni, ni)),
					_List_fromArray(
						[
							A2($MacCASOutreach$graphicsvg$GraphicSVG$wedgeHelper, r, frac),
							_Utils_Tuple2(0, 0)
						]))) : _List_Nil);
	});
var $author$project$ShapeCreator$stencilFun = function (m) {
	var _v0 = m.bT;
	switch (_v0) {
		case 0:
			return $MacCASOutreach$graphicsvg$GraphicSVG$circle(m.dl);
		case 7:
			return $MacCASOutreach$graphicsvg$GraphicSVG$square(m.dl);
		case 1:
			return A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, m.dl, m.cS);
		case 2:
			return A3($MacCASOutreach$graphicsvg$GraphicSVG$roundedRect, m.dl, m.cS, m.c9);
		case 3:
			return A2($MacCASOutreach$graphicsvg$GraphicSVG$oval, m.dl, m.cS);
		case 4:
			return A2($MacCASOutreach$graphicsvg$GraphicSVG$ngon, m.da, m.dl);
		case 5:
			return $MacCASOutreach$graphicsvg$GraphicSVG$triangle(m.dl);
		case 6:
			return A2($MacCASOutreach$graphicsvg$GraphicSVG$wedge, m.dl, m.c_);
		case 9:
			return A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$curve,
				_Utils_Tuple2(0, 0),
				_List_fromArray(
					[
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$Pull,
						_Utils_Tuple2(10, 0),
						_Utils_Tuple2(20, -10))
					]));
		case 10:
			return $MacCASOutreach$graphicsvg$GraphicSVG$polygon(
				_List_fromArray(
					[
						_Utils_Tuple2(0, 0),
						_Utils_Tuple2(0, -10),
						_Utils_Tuple2(30, 0)
					]));
		case 11:
			return $MacCASOutreach$graphicsvg$GraphicSVG$openPolygon(
				_List_fromArray(
					[
						_Utils_Tuple2(0, 0),
						_Utils_Tuple2(0, -10),
						_Utils_Tuple2(30, 0)
					]));
		default:
			return $MacCASOutreach$graphicsvg$GraphicSVG$text(m.aP);
	}
};
var $author$project$ShapeCreator$shapeFun = function (m) {
	return (m.aZ ? $MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent(m.ad) : function (x) {
		return x;
	})(
		(m.a1 ? $MacCASOutreach$graphicsvg$GraphicSVG$scaleY(m._) : function (x) {
			return x;
		})(
			(m.a0 ? $MacCASOutreach$graphicsvg$GraphicSVG$scaleX(m._) : function (x) {
				return x;
			})(
				(m.a$ ? $MacCASOutreach$graphicsvg$GraphicSVG$scale(m._) : function (x) {
					return x;
				})(
					(m.a_ ? $MacCASOutreach$graphicsvg$GraphicSVG$rotate(
						$elm$core$Basics$degrees(m.aI)) : function (x) {
						return x;
					})(
						(m.b5 ? $MacCASOutreach$graphicsvg$GraphicSVG$move(
							_Utils_Tuple2(m.cv, m.bg)) : function (x) {
							return x;
						})(
							function () {
								var _v0 = m.aV;
								if (!_v0) {
									return A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$author$project$ShapeCreator$colourFun(m),
										$author$project$ShapeCreator$stencilFun(m));
								} else {
									return A3(
										$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
										$author$project$ShapeCreator$lineStyleFun(m),
										$author$project$ShapeCreator$colourFun(m),
										$author$project$ShapeCreator$stencilFun(m));
								}
							}()))))));
};
var $author$project$ShapeCreator$Draw = function (a) {
	return {$: 2, a: a};
};
var $author$project$ShapeCreator$LStyle = {$: 3};
var $author$project$ShapeCreator$Outlined = 1;
var $author$project$ShapeCreator$stampString = F2(
	function (m, stamp) {
		if (!stamp) {
			return 'filled ';
		} else {
			return 'outlined ';
		}
	});
var $author$project$ShapeCreator$styleString = function (m) {
	return '(' + (function () {
		var _v0 = m.br;
		switch (_v0) {
			case 0:
				return 'solid ';
			case 1:
				return 'dotted ';
			case 2:
				return 'dashed ';
			case 3:
				return 'longdash ';
			default:
				return 'dotdash ';
		}
	}() + ($elm$core$String$fromFloat(m.I) + ')'));
};
var $author$project$ShapeCreator$time2 = F5(
	function (model, ss, w, h, shape) {
		return _Utils_eq(ss, model.aV) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4(
						$MacCASOutreach$graphicsvg$GraphicSVG$rgba,
						255,
						137,
						5,
						0.6 + (0.4 * $elm$core$Basics$sin((5 * model.aO) - 0.5))),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h)),
					shape
				])) : shape;
	});
var $author$project$ShapeCreator$stamps = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, -1),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 130, 30)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, 14),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 95, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-85, 11),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$ShapeCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('2. Fill it or Outline it!')))))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (ss, y) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-40, y),
								A5(
									$author$project$ShapeCreator$time2,
									model,
									ss,
									130,
									10,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-63, -2.5),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											$author$project$ShapeCreator$Draw(ss),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$black,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$size,
													10,
													$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
														$MacCASOutreach$graphicsvg$GraphicSVG$text(
															A2($author$project$ShapeCreator$stampString, model, ss)))))))));
						}),
					_List_fromArray(
						[0, 1]),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-10) * x;
						},
						A2($elm$core$List$range, 0, 20)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
				$author$project$ShapeCreator$LStyle,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-43, -12.5),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$black,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$size,
							10,
							$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
								$MacCASOutreach$graphicsvg$GraphicSVG$text(
									$author$project$ShapeCreator$styleString(model)))))))
			]));
};
var $author$project$ShapeCreator$BezierPath = 9;
var $author$project$ShapeCreator$Ngon = 4;
var $author$project$ShapeCreator$OpenPolygon = 11;
var $author$project$ShapeCreator$Oval = 3;
var $author$project$ShapeCreator$Polygon = 10;
var $author$project$ShapeCreator$Rect = 1;
var $author$project$ShapeCreator$RoundedRect = 2;
var $author$project$ShapeCreator$Square = 7;
var $author$project$ShapeCreator$Sten = function (a) {
	return {$: 1, a: a};
};
var $author$project$ShapeCreator$Text = 8;
var $author$project$ShapeCreator$Triangle = 5;
var $author$project$ShapeCreator$Wedge = 6;
var $author$project$ShapeCreator$stencilString = F2(
	function (m, shape) {
		switch (shape) {
			case 0:
				return 'circle ' + $elm$core$String$fromFloat(m.dl);
			case 7:
				return 'square ' + $elm$core$String$fromFloat(m.dl);
			case 1:
				return 'rect ' + ($elm$core$String$fromFloat(m.dl) + (' ' + $elm$core$String$fromFloat(m.cS)));
			case 2:
				return 'roundedRect ' + ($elm$core$String$fromFloat(m.dl) + (' ' + ($elm$core$String$fromFloat(m.cS) + (' ' + $elm$core$String$fromFloat(m.c9)))));
			case 3:
				return 'oval ' + ($elm$core$String$fromFloat(m.dl) + (' ' + $elm$core$String$fromFloat(m.cS)));
			case 4:
				return 'ngon ' + ($elm$core$String$fromInt(m.da) + (' ' + $elm$core$String$fromFloat(m.dl)));
			case 5:
				return 'triangle ' + $elm$core$String$fromFloat(m.dl);
			case 6:
				return 'wedge ' + ($elm$core$String$fromFloat(m.dl) + (' ' + $elm$core$String$fromFloat(m.c_)));
			case 9:
				return 'curve (0,0) [Pull (10,0) (20,-10)]';
			case 10:
				return 'polygon [(0,0),(0,-10),(30,0)]';
			case 11:
				return 'openPolygon [(0,0),(0,-10),(30,0)]';
			default:
				return 'text \"' + (m.aP + '\"');
		}
	});
var $author$project$ShapeCreator$time1 = F5(
	function (model, ss, w, h, shape) {
		return _Utils_eq(ss, model.bT) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4(
						$MacCASOutreach$graphicsvg$GraphicSVG$rgba,
						255,
						137,
						5,
						0.6 + (0.4 * $elm$core$Basics$sin(5 * model.aO))),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h)),
					shape
				])) : shape;
	});
var $author$project$ShapeCreator$stencils = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(0, -47),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 210, 120)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(0, 13),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 75, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-35, 10),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$ShapeCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('1. Pick a Stencil!')))))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (ss, y) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(0, y),
								A5(
									$author$project$ShapeCreator$time1,
									model,
									ss,
									210,
									10,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-103, -2.5),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											$author$project$ShapeCreator$Sten(ss),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$black,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$size,
													10,
													$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
														$MacCASOutreach$graphicsvg$GraphicSVG$text(
															A2($author$project$ShapeCreator$stencilString, model, ss)))))))));
						}),
					_List_fromArray(
						[0, 7, 1, 2, 8, 4, 5, 6, 3, 9, 10, 11]),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-10) * x;
						},
						A2($elm$core$List$range, 0, 20))))
			]));
};
var $author$project$ShapeCreator$MakeTransparent = 5;
var $author$project$ShapeCreator$Move = 0;
var $author$project$ShapeCreator$Rotate = 1;
var $author$project$ShapeCreator$Scale = 2;
var $author$project$ShapeCreator$ScaleX = 3;
var $author$project$ShapeCreator$ScaleY = 4;
var $author$project$ShapeCreator$Toggle = function (a) {
	return {$: 6, a: a};
};
var $author$project$ShapeCreator$time4 = F5(
	function (model, t, w, h, shape) {
		return function () {
			switch (t) {
				case 0:
					return model.b5;
				case 1:
					return model.a_;
				case 2:
					return model.a$;
				case 3:
					return model.a0;
				case 4:
					return model.a1;
				default:
					return model.aZ;
			}
		}() ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4(
						$MacCASOutreach$graphicsvg$GraphicSVG$rgba,
						255,
						137,
						5,
						0.6 + (0.4 * $elm$core$Basics$sin((5 * model.aO) - 1.5))),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h)),
					shape
				])) : shape;
	});
var $author$project$ShapeCreator$transformString = F2(
	function (m, t) {
		switch (t) {
			case 0:
				return '|> move (' + ($elm$core$String$fromFloat(m.cv) + (',' + ($elm$core$String$fromFloat(m.bg) + ')')));
			case 1:
				return '|> rotate (degrees ' + ($elm$core$String$fromFloat(m.aI) + ')');
			case 2:
				return '|> scale ' + $elm$core$String$fromFloat(m._);
			case 3:
				return '|> scaleX ' + $elm$core$String$fromFloat(m.aB);
			case 4:
				return '|> scaleY ' + $elm$core$String$fromFloat(m.aC);
			default:
				return '|> makeTransparent ' + $elm$core$String$fromFloat(m.ad);
		}
	});
var $author$project$ShapeCreator$transforms = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-35, -21),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 140, 70)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-45, 14),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 95, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-85, 11),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$ShapeCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('4. Apply Transforms!')))))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (ss, y) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-35, y),
								A5(
									$author$project$ShapeCreator$time4,
									model,
									ss,
									140,
									10,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-68, -2.5),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											$author$project$ShapeCreator$Toggle(ss),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$black,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$size,
													10,
													$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
														$MacCASOutreach$graphicsvg$GraphicSVG$text(
															A2($author$project$ShapeCreator$transformString, model, ss)))))))));
						}),
					_List_fromArray(
						[2, 3, 4, 1, 0, 5]),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-10) * x;
						},
						A2($elm$core$List$range, 0, 20))))
			]));
};
var $author$project$ShapeCreator$tweaks = $MacCASOutreach$graphicsvg$GraphicSVG$group(
	_List_fromArray(
		[
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-35, -61),
			A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
				$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
				$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 140, 150)))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-60, 14),
			A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
				$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
				$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$white,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 55, 12)))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-85, 11),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$author$project$ShapeCreator$titleColour,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					10,
					$MacCASOutreach$graphicsvg$GraphicSVG$italic(
						$MacCASOutreach$graphicsvg$GraphicSVG$serif(
							$MacCASOutreach$graphicsvg$GraphicSVG$text('5. Tweak it!')))))),
			$MacCASOutreach$graphicsvg$GraphicSVG$group(
			A3(
				$elm$core$List$map2,
				F2(
					function (_v0, _v1) {
						var str = _v0.a;
						var msg = _v0.b;
						var x = _v1.a;
						var y = _v1.b;
						return A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2((-68) + x, (-2.5) + y),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								msg,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$black,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$size,
										10,
										$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
											$MacCASOutreach$graphicsvg$GraphicSVG$text(str))))));
					}),
				_List_fromArray(
					[
						_Utils_Tuple2(
						'up',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{bg: m.bg + 10});
							})),
						_Utils_Tuple2(
						'down',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{bg: m.bg - 10});
							})),
						_Utils_Tuple2(
						'left',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{cv: m.cv - 10});
							})),
						_Utils_Tuple2(
						'right',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{cv: m.cv + 10});
							})),
						_Utils_Tuple2(
						'clockwise',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{aI: m.aI - 30});
							})),
						_Utils_Tuple2(
						'counter',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{aI: m.aI + 30});
							})),
						_Utils_Tuple2(
						'wider',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										dl: (m.dl < 100) ? (m.dl + 10) : 100
									});
							})),
						_Utils_Tuple2(
						'narrower',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										dl: (m.dl > 10) ? (m.dl - 10) : 10
									});
							})),
						_Utils_Tuple2(
						'taller',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										cS: (m.cS < 100) ? (m.cS + 10) : 100
									});
							})),
						_Utils_Tuple2(
						'shorter',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										cS: (m.cS > 10) ? (m.cS - 10) : 10
									});
							})),
						_Utils_Tuple2(
						'mouthier',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										c_: (m.c_ > 0) ? (m.c_ - 0.125) : 0
									});
							})),
						_Utils_Tuple2(
						'mouthiless',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										c_: (m.c_ < 1) ? (m.c_ + 0.125) : 1
									});
							})),
						_Utils_Tuple2(
						'rounder',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										c9: (m.c9 < 50) ? (m.c9 + 5) : 50
									});
							})),
						_Utils_Tuple2(
						'sharper',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										c9: (m.c9 > 0) ? (m.c9 - 5) : 0
									});
							})),
						_Utils_Tuple2(
						'thicker',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										I: (m.I < 10) ? (m.I + 0.5) : 10
									});
							})),
						_Utils_Tuple2(
						'thinner',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										I: (m.I > 0.5) ? (m.I - 0.5) : 0.5
									});
							})),
						_Utils_Tuple2(
						'more red',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										G: (m.G < 248) ? (m.G + 7) : 255
									});
							})),
						_Utils_Tuple2(
						'less red',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										G: (m.G > 8) ? (m.G - 8) : 0
									});
							})),
						_Utils_Tuple2(
						'more green',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										D: (m.D < 248) ? (m.D + 7) : 255
									});
							})),
						_Utils_Tuple2(
						'less green',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										D: (m.D > 8) ? (m.D - 8) : 0
									});
							})),
						_Utils_Tuple2(
						'more blue',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										C: (m.C < 248) ? (m.C + 7) : 255
									});
							})),
						_Utils_Tuple2(
						'less blue',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										C: (m.C > 8) ? (m.C - 8) : 0
									});
							})),
						_Utils_Tuple2(
						'solider',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										ad: (m.ad < 1) ? (m.ad + 0.125) : 1
									});
							})),
						_Utils_Tuple2(
						'ghostier',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										ad: (m.ad > 0) ? (m.ad - 0.125) : 0
									});
							})),
						_Utils_Tuple2(
						'bigger',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										_: (m._ < 3) ? (m._ + 0.25) : 3,
										aB: (m.aB < 3) ? (m.aB + 0.25) : 3,
										aC: (m.aC < 3) ? (m.aC + 0.25) : 3
									});
							})),
						_Utils_Tuple2(
						'smaller',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										_: (_Utils_cmp(m._, -3) > 0) ? (m._ - 0.25) : (-3),
										aB: (_Utils_cmp(m.aB, -3) > 0) ? (m.aB - 0.25) : (-3),
										aC: (_Utils_cmp(m.aC, -3) > 0) ? (m.aC - 0.25) : (-3)
									});
							})),
						_Utils_Tuple2(
						'hello?',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										aP: $author$project$ShapeCreator$cycleTxt(m.aP)
									});
							})),
						_Utils_Tuple2(
						'take sides',
						$author$project$ShapeCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										da: (m.da > 20) ? 3 : (m.da + 1)
									});
							}))
					]),
				$elm$core$List$concat(
					A2(
						$elm$core$List$map,
						function (idx) {
							return _List_fromArray(
								[
									_Utils_Tuple2(-30, (-10) * idx),
									_Utils_Tuple2(40, (-10) * idx)
								]);
						},
						A2($elm$core$List$range, 0, 20)))))
		]));
var $author$project$ShapeCreator$copiable = function (str) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$filled,
		$MacCASOutreach$graphicsvg$GraphicSVG$black,
		A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$size,
			10,
			$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
				$MacCASOutreach$graphicsvg$GraphicSVG$selectable(
					$MacCASOutreach$graphicsvg$GraphicSVG$text(str)))));
};
var $author$project$ShapeCreator$yourCode = function (m) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-20, -30),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 160, 85)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-55, 14),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-90, 11),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$ShapeCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('5. Your code!')))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-85, 0),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
					_Utils_ap(
						_List_fromArray(
							[
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-10, 0),
								$author$project$ShapeCreator$copiable(
									A2($author$project$ShapeCreator$stencilString, m, m.bT))),
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(0, -10),
								$author$project$ShapeCreator$copiable(
									'  |> ' + (A2($author$project$ShapeCreator$stampString, m, m.aV) + (((m.aV === 1) ? ($author$project$ShapeCreator$styleString(m) + ' ') : '') + A2($author$project$ShapeCreator$clrString, m, m.bj)))))
							]),
						A3(
							$elm$core$List$map2,
							F2(
								function (str, y) {
									return A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(0, y),
										$author$project$ShapeCreator$copiable(str));
								}),
							$elm$core$List$concat(
								A2(
									$elm$core$List$map,
									function (_v0) {
										var flag = _v0.a;
										var t = _v0.b;
										return flag ? _List_fromArray(
											[
												'  ' + A2($author$project$ShapeCreator$transformString, m, t)
											]) : _List_Nil;
									},
									_List_fromArray(
										[
											_Utils_Tuple2(m.a$, 2),
											_Utils_Tuple2(m.a0, 3),
											_Utils_Tuple2(m.a1, 4),
											_Utils_Tuple2(m.a_, 1),
											_Utils_Tuple2(m.b5, 0),
											_Utils_Tuple2(m.aZ, 5)
										]))),
							_List_fromArray(
								[-20, -30, -40, -50, -60, -70, -80])))))
			]));
};
var $author$project$ShapeCreator$view = function (model) {
	return _List_fromArray(
		[
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
			0.25,
			A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$graphPaperCustom,
				10,
				1,
				A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 255, 137, 5))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$filled,
			$MacCASOutreach$graphicsvg$GraphicSVG$brown,
			A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 512, 0.5)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$filled,
			$MacCASOutreach$graphicsvg$GraphicSVG$brown,
			A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 512)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(0, 100),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$brown,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 4, 0.5))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(3, 100),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$brown,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(0,100)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(0, -100),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$brown,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 4, 0.5))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(3, -100),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$brown,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(0,-100)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-100, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$brown,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-100, 3),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$brown,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(-100,0)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(100, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$brown,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(100, 3),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$brown,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(100,0)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-200, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$brown,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-200, 3),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$brown,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(-200,0)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(200, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$brown,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(200, 3),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$brown,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(200,0)')))),
			$author$project$ShapeCreator$shapeFun(model),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-150, 170),
			$author$project$ShapeCreator$stencils(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-40, 165),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$black,
				$MacCASOutreach$graphicsvg$GraphicSVG$text('|>'))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(80, 169),
			$author$project$ShapeCreator$stamps(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(230, 169),
			$author$project$ShapeCreator$colours(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-150, 30),
			$author$project$ShapeCreator$transforms(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-150, -50),
			$author$project$ShapeCreator$tweaks),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(0, -115),
			$author$project$ShapeCreator$yourCode(model))
		]);
};
var $author$project$SinCreator$AmplitudeDown = 1;
var $author$project$SinCreator$AmplitudeUp = 0;
var $author$project$SinCreator$B = {$: 5};
var $author$project$SinCreator$BScaleMinus = {$: 29};
var $author$project$SinCreator$BScalePlus = {$: 28};
var $author$project$SinCreator$BlueDown = 13;
var $author$project$SinCreator$BlueUp = 12;
var $author$project$SinCreator$ButtonDown = function (a) {
	return {$: 30, a: a};
};
var $author$project$SinCreator$FrequencyDown = 3;
var $author$project$SinCreator$FrequencyUp = 2;
var $author$project$SinCreator$G = {$: 4};
var $author$project$SinCreator$GScaleMinus = {$: 27};
var $author$project$SinCreator$GScalePlus = {$: 26};
var $author$project$SinCreator$GreenDown = 15;
var $author$project$SinCreator$GreenUp = 14;
var $author$project$SinCreator$R = {$: 3};
var $author$project$SinCreator$RScaleMinus = {$: 25};
var $author$project$SinCreator$RScalePlus = {$: 24};
var $author$project$SinCreator$RedDown = 11;
var $author$project$SinCreator$RedUp = 10;
var $author$project$SinCreator$ShiftDown = 5;
var $author$project$SinCreator$ShiftUp = 4;
var $author$project$SinCreator$TransM = function (a) {
	return {$: 1, a: a};
};
var $author$project$SinCreator$UDilationMinus = {$: 10};
var $author$project$SinCreator$UDilationPlus = {$: 7};
var $author$project$SinCreator$UScaleMinus = {$: 9};
var $author$project$SinCreator$UScalePlus = {$: 6};
var $author$project$SinCreator$UShiftMinus = {$: 11};
var $author$project$SinCreator$UShiftPlus = {$: 8};
var $author$project$SinCreator$UTransforms = {$: 22};
var $author$project$SinCreator$UTransformsReverse = {$: 23};
var $author$project$SinCreator$applyTransforms = F2(
	function (tr, model) {
		var u = model.bK;
		switch (tr) {
			case 0:
				return $MacCASOutreach$graphicsvg$GraphicSVG$scale((model.aF + model.b) / 10);
			case 1:
				return $MacCASOutreach$graphicsvg$GraphicSVG$move(
					_Utils_Tuple2(model.be, 0));
			case 2:
				return $MacCASOutreach$graphicsvg$GraphicSVG$move(
					_Utils_Tuple2(0, model.aF));
			case 3:
				return $MacCASOutreach$graphicsvg$GraphicSVG$move(
					_Utils_Tuple2(model.be, model.aF));
			case 4:
				return $MacCASOutreach$graphicsvg$GraphicSVG$rotate(u / 10);
			case 5:
				return $MacCASOutreach$graphicsvg$GraphicSVG$scaleX((model.be + model.b) / 10);
			case 6:
				return $MacCASOutreach$graphicsvg$GraphicSVG$scaleY((model.aF + model.b) / 10);
			case 7:
				return $MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent(u);
			default:
				return $MacCASOutreach$graphicsvg$GraphicSVG$move(
					_Utils_Tuple2(model.be, 0));
		}
	});
var $author$project$SinCreator$applyTransformsText = function (tr) {
	switch (tr) {
		case 1:
			return ' move x ';
		case 2:
			return ' move y ';
		case 3:
			return ' move in a circle ';
		case 0:
			return ' scale ';
		case 4:
			return ' rotate ';
		case 5:
			return ' scaleX ';
		case 6:
			return ' scaleY ';
		case 7:
			return ' makeTransparent ';
		default:
			return ' editable Y Sin ';
	}
};
var $elm$core$String$right = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(
			$elm$core$String$slice,
			-n,
			$elm$core$String$length(string),
			string);
	});
var $author$project$SinCreator$showDigits = F2(
	function (width, x) {
		return A2(
			$elm$core$String$right,
			width,
			'      ' + $elm$core$String$fromFloat(x));
	});
var $author$project$SinCreator$cosinString = function (model) {
	var fraction = (((model.L / 8) * 2) < 0) ? A2($author$project$SinCreator$showDigits, 5, (model.L / 8) * 2) : ('+' + A2($author$project$SinCreator$showDigits, 4, (model.L / 8) * 2));
	return A2($author$project$SinCreator$showDigits, 2, model.k) + ('*model.time' + (fraction + '*Pi)'));
};
var $author$project$SinCreator$applyTransformsYourCode = F2(
	function (model, tr) {
		switch (tr) {
			case 1:
				return '|> move (' + ($elm$core$String$fromFloat(model.b) + '*cos(model.time) , 0)');
			case 2:
				return '|> move (0 , ' + ($elm$core$String$fromFloat(model.b) + '*sin(model.time))');
			case 3:
				return '|> move (' + ($elm$core$String$fromFloat(model.b) + ('*sin(' + ($author$project$SinCreator$cosinString(model) + (', ' + ($elm$core$String$fromFloat(model.b) + ('*cos(' + $author$project$SinCreator$cosinString(model)))))));
			case 0:
				return '|> scale ' + ($elm$core$String$fromFloat(model.b) + ('*sin(' + $author$project$SinCreator$cosinString(model)));
			case 4:
				return '|> rotate ' + ($elm$core$String$fromFloat(model.b) + ('*sin(' + $author$project$SinCreator$cosinString(model)));
			case 5:
				return '|> scaleX ' + ($elm$core$String$fromFloat(model.b) + ('*cos(' + $author$project$SinCreator$cosinString(model)));
			case 6:
				return '|> scaleY ' + ($elm$core$String$fromFloat(model.b) + ('*sin(' + $author$project$SinCreator$cosinString(model)));
			case 7:
				return '|> makeTransparent ' + ($elm$core$String$fromFloat(model.b) + ('*sin(' + $author$project$SinCreator$cosinString(model)));
			default:
				return '|> move (' + ($elm$core$String$fromFloat(model.m) + ('*cos(model.time) , ' + ('0' + ')')));
		}
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$bold = function (stencil) {
	if (stencil.$ === 7) {
		var _v1 = stencil.a;
		var si = _v1.a;
		var bo = _v1.b;
		var i = _v1.c;
		var u = _v1.d;
		var s = _v1.e;
		var sel = _v1.f;
		var f = _v1.g;
		var c = _v1.h;
		var str = stencil.b;
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$Text,
			A8($MacCASOutreach$graphicsvg$GraphicSVG$Face, si, true, i, u, s, sel, f, c),
			str);
	} else {
		var a = stencil;
		return a;
	}
};
var $author$project$SinCreator$copiable = function (str) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$filled,
		$MacCASOutreach$graphicsvg$GraphicSVG$black,
		A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$size,
			6,
			$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
				$MacCASOutreach$graphicsvg$GraphicSVG$selectable(
					$MacCASOutreach$graphicsvg$GraphicSVG$text(str)))));
};
var $author$project$SinCreator$downArrow = A2(
	$MacCASOutreach$graphicsvg$GraphicSVG$filled,
	A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 0, 0, 0.6),
	$MacCASOutreach$graphicsvg$GraphicSVG$polygon(
		_List_fromArray(
			[
				_Utils_Tuple2(-6, 0),
				_Utils_Tuple2(6, 0),
				_Utils_Tuple2(0, -12)
			])));
var $author$project$SinCreator$textTrig = function (f) {
	if (!f) {
		return 'sin';
	} else {
		return 'cos';
	}
};
var $author$project$SinCreator$functionText = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-120, 0),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
							$MacCASOutreach$graphicsvg$GraphicSVG$text(
								A2($author$project$SinCreator$showDigits, 2, model.b) + ('*' + ($author$project$SinCreator$textTrig(model.aD) + ('(' + $author$project$SinCreator$cosinString(model)))))))))
			]));
};
var $author$project$SinCreator$moveText = function (mv) {
	switch (mv) {
		case 0:
			return 'u';
		case 1:
			return 'u';
		case 2:
			return '-u';
		case 3:
			return 'v';
		default:
			return '-v';
	}
};
var $author$project$SinCreator$showFun = F3(
	function (f, u, v) {
		switch (f) {
			case 0:
				return 'u';
			case 1:
				return 'u';
			default:
				return 'v';
		}
	});
var $author$project$SinCreator$numGraphPoints = function (model) {
	return $elm$core$Basics$round(2505);
};
var $author$project$SinCreator$sinCurve = function (model) {
	var points = A3(
		$elm$core$List$map2,
		F2(
			function (x, y) {
				return _Utils_Tuple2(x, y);
			}),
		model.bq,
		A2($elm$core$List$drop, 1, model.bq));
	return A2(
		$elm$core$List$take,
		$author$project$SinCreator$numGraphPoints(model),
		A2(
			$elm$core$List$map,
			function (_v0) {
				var _v1 = _v0.a;
				var a = _v1.a;
				var b = _v1.b;
				var col1 = _v1.c;
				var _v2 = _v0.b;
				var c = _v2.a;
				var d = _v2.b;
				var col2 = _v2.c;
				return A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					col1,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$line,
						_Utils_Tuple2(a, b),
						_Utils_Tuple2(c, d)));
			},
			points));
};
var $author$project$SinCreator$titleColour = A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 200, 0, 0, 0.95);
var $author$project$SinCreator$trigGraphAxis = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(165, -20),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 50))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(195, -20),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(0.5),
					$MacCASOutreach$graphicsvg$GraphicSVG$brown,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 180, 60))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(165 + (model.bE / 2), -20),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, model.bE, 0.5)))
			]));
};
var $MacCASOutreach$graphicsvg$GraphicSVG$underline = function (stencil) {
	if (stencil.$ === 7) {
		var _v1 = stencil.a;
		var si = _v1.a;
		var bo = _v1.b;
		var i = _v1.c;
		var u = _v1.d;
		var s = _v1.e;
		var sel = _v1.f;
		var f = _v1.g;
		var c = _v1.h;
		var str = stencil.b;
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$Text,
			A8($MacCASOutreach$graphicsvg$GraphicSVG$Face, si, bo, i, true, s, sel, f, c),
			str);
	} else {
		var a = stencil;
		return a;
	}
};
var $author$project$SinCreator$upArrow = A2(
	$MacCASOutreach$graphicsvg$GraphicSVG$filled,
	A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 0, 0, 0.6),
	$MacCASOutreach$graphicsvg$GraphicSVG$polygon(
		_List_fromArray(
			[
				_Utils_Tuple2(-6, 0),
				_Utils_Tuple2(6, 0),
				_Utils_Tuple2(0, 12)
			])));
var $author$project$SinCreator$view = function (model) {
	var x2 = (model.ac === 7) ? 116 : 81;
	var x1 = (model.ac === 7) ? 90 : 45;
	var waveHeader = A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
		1,
		A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$filled,
			$MacCASOutreach$graphicsvg$GraphicSVG$brown,
			$MacCASOutreach$graphicsvg$GraphicSVG$serif(
				$MacCASOutreach$graphicsvg$GraphicSVG$underline(
					$MacCASOutreach$graphicsvg$GraphicSVG$bold(
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$size,
							20,
							$MacCASOutreach$graphicsvg$GraphicSVG$text('WaveCreator - Shape Animator')))))));
	var v = model.bW;
	var uScale = model.b;
	var uArg = model.aE;
	var u = model.bK;
	var yourCodeGroup = $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(100, 20),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
					0.25,
					A3(
						$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
						$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
						$MacCASOutreach$graphicsvg$GraphicSVG$red,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 200, 100)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(0, 60),
				$author$project$SinCreator$copiable('--Add these new definitions to your code')),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(0, 50),
				$author$project$SinCreator$copiable(
					'u = ' + ($elm$core$String$fromFloat(model.b) + ('*' + ($author$project$SinCreator$textTrig(model.aD) + ('(' + ($elm$core$String$fromFloat(model.k) + ('*model.time+' + ($elm$core$String$fromFloat(model.L) + ')'))))))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(0, 30),
				$author$project$SinCreator$copiable('mySquare = square 15')),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(35, 20),
				$author$project$SinCreator$copiable(
					'  |> outlined (solid 0.25) rgb (' + ($elm$core$String$fromFloat(model.q) + ('*' + (A3($author$project$SinCreator$showFun, model.bb, u, v) + (' ' + ($elm$core$String$fromFloat(model.p) + ('*' + (A3($author$project$SinCreator$showFun, model.aY, u, v) + (' ' + ($elm$core$String$fromFloat(model.n) + ('*' + (A3($author$project$SinCreator$showFun, model.aS, u, v) + ')'))))))))))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(35, 10),
				$author$project$SinCreator$copiable(
					'  ' + A2($author$project$SinCreator$applyTransformsYourCode, model, model.ac))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(35, 0),
				$author$project$SinCreator$copiable(
					'  |> move(' + ($author$project$SinCreator$moveText(model.b9) + (',' + ($author$project$SinCreator$moveText(model.ca) + ')'))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(0, -10),
				$author$project$SinCreator$copiable('--Add the following code to your shapes:')),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(10, -20),
				$author$project$SinCreator$copiable('mySquare'))
			]));
	var tt = function (str) {
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$filled,
			$author$project$SinCreator$titleColour,
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$size,
				10,
				$MacCASOutreach$graphicsvg$GraphicSVG$italic(
					$MacCASOutreach$graphicsvg$GraphicSVG$serif(
						$MacCASOutreach$graphicsvg$GraphicSVG$text(str)))));
	};
	var transformsGraphicsGroup = $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(40, 60),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
					0.25,
					A3(
						$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
						$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
						$MacCASOutreach$graphicsvg$GraphicSVG$red,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 240, 230)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(45, 90),
				A3(
					$author$project$SinCreator$applyTransforms,
					model.ac,
					model,
					A3(
						$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
						$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
						A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, model.ba, model.aX, model.aR),
						$MacCASOutreach$graphicsvg$GraphicSVG$square(15)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(25, 50),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
					_List_fromArray(
						[
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(4, 105),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$black,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$size,
									10,
									$MacCASOutreach$graphicsvg$GraphicSVG$text(
										$author$project$SinCreator$applyTransformsText(model.ac))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
							model.bH,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyEnter,
								$author$project$SinCreator$TransM(
									function (m) {
										return _Utils_update(
											m,
											{bH: 1});
									}),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyLeave,
									$author$project$SinCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{bH: 0.25});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-70, 105),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											$author$project$SinCreator$UTransformsReverse,
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
												$elm$core$Basics$degrees(180),
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$filled,
													A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 255, 10, 10),
													$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8)))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
							model.bI,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyEnter,
								$author$project$SinCreator$TransM(
									function (m) {
										return _Utils_update(
											m,
											{bI: 1});
									}),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyLeave,
									$author$project$SinCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{bI: 0.25});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(100, 105),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											$author$project$SinCreator$UTransforms,
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 255, 10, 10),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8)))))))
						])))
			]));
	var titlesText = $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(50, 0),
				tt('2. Modify your function!')),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-200, 110),
				tt('1. Choose an animation!')),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(40, 110),
				tt('3. Your generated code!'))
			]));
	var setofTriangles = $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
				$author$project$SinCreator$ButtonDown(16),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
					$author$project$SinCreator$ButtonDown(2),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(-67, -5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$notifyTap, $author$project$SinCreator$UDilationPlus, $author$project$SinCreator$upArrow)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
				$author$project$SinCreator$ButtonDown(16),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
					$author$project$SinCreator$ButtonDown(0),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(-111, -5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$notifyTap, $author$project$SinCreator$UScalePlus, $author$project$SinCreator$upArrow)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
				$author$project$SinCreator$ButtonDown(16),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
					$author$project$SinCreator$ButtonDown(4),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(17, -5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$notifyTap, $author$project$SinCreator$UShiftPlus, $author$project$SinCreator$upArrow)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
				$author$project$SinCreator$ButtonDown(16),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
					$author$project$SinCreator$ButtonDown(3),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(-67, -20),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$notifyTap, $author$project$SinCreator$UDilationMinus, $author$project$SinCreator$downArrow)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
				$author$project$SinCreator$ButtonDown(16),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
					$author$project$SinCreator$ButtonDown(1),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(-111, -20),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$notifyTap, $author$project$SinCreator$UScaleMinus, $author$project$SinCreator$downArrow)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
				$author$project$SinCreator$ButtonDown(16),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
					$author$project$SinCreator$ButtonDown(5),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(17, -20),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$notifyTap, $author$project$SinCreator$UShiftMinus, $author$project$SinCreator$downArrow))))
			]));
	var rgbGraphics = $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(43, -5),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
					0.25,
					A3(
						$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
						$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
						$MacCASOutreach$graphicsvg$GraphicSVG$red,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 140, 50)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-25, 0),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
							$MacCASOutreach$graphicsvg$GraphicSVG$text('rgb '))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
				model.bC,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyLeave,
					$author$project$SinCreator$TransM(
						function (m) {
							return _Utils_update(
								m,
								{bC: 0.25});
						}),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyEnter,
						$author$project$SinCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{bC: 1});
							}),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$SinCreator$R,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-5, 0),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$black,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$size,
										10,
										$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
											$MacCASOutreach$graphicsvg$GraphicSVG$text(
												'(' + ($elm$core$String$fromFloat(model.q) + ('*' + (A3($author$project$SinCreator$showFun, model.bb, u, v) + ')')))))))))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
				model.bz,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyLeave,
					$author$project$SinCreator$TransM(
						function (m) {
							return _Utils_update(
								m,
								{bz: 0.25});
						}),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyEnter,
						$author$project$SinCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{bz: 1});
							}),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$SinCreator$G,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(35, 0),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$black,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$size,
										10,
										$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
											$MacCASOutreach$graphicsvg$GraphicSVG$text(
												'(' + ($elm$core$String$fromFloat(model.p) + ('*' + (A3($author$project$SinCreator$showFun, model.aY, u, v) + ')')))))))))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
				model.bx,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyLeave,
					$author$project$SinCreator$TransM(
						function (m) {
							return _Utils_update(
								m,
								{bx: 0.25});
						}),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyEnter,
						$author$project$SinCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{bx: 1});
							}),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$SinCreator$B,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(73, 0),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$black,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$size,
										10,
										$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
											$MacCASOutreach$graphicsvg$GraphicSVG$text(
												'(' + ($elm$core$String$fromFloat(model.n) + ('*' + (A3($author$project$SinCreator$showFun, model.aS, u, v) + ')')))))))))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
				$author$project$SinCreator$ButtonDown(16),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
					$author$project$SinCreator$ButtonDown(10),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
						$elm$core$Basics$degrees(90),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(5, -10),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$SinCreator$RScalePlus,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 255, 10, 10),
									$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
				$author$project$SinCreator$ButtonDown(16),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
					$author$project$SinCreator$ButtonDown(11),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(15, -10),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
							$elm$core$Basics$degrees(-90),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$SinCreator$RScaleMinus,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 180, 140, 140),
									$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
				$author$project$SinCreator$ButtonDown(16),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
					$author$project$SinCreator$ButtonDown(14),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(43, -10),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
							$elm$core$Basics$degrees(90),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$SinCreator$GScalePlus,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 10, 255, 10),
									$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
				$author$project$SinCreator$ButtonDown(16),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
					$author$project$SinCreator$ButtonDown(15),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(53, -10),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
							$elm$core$Basics$degrees(-90),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$SinCreator$GScaleMinus,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 140, 180, 140),
									$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
				$author$project$SinCreator$ButtonDown(16),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
					$author$project$SinCreator$ButtonDown(12),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(80, -10),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
							$elm$core$Basics$degrees(90),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$SinCreator$BScalePlus,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 10, 10, 255),
									$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
				$author$project$SinCreator$ButtonDown(16),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
					$author$project$SinCreator$ButtonDown(13),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(90, -10),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
							$elm$core$Basics$degrees(-90),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$SinCreator$BScaleMinus,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 140, 140, 180),
									$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8)))))))
			]));
	var notTrigCycleU = (!model.aD) ? $elm$core$Basics$cos : $elm$core$Basics$sin;
	var instructBox = $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(100, 45),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
					0.25,
					A3(
						$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
						$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
						$MacCASOutreach$graphicsvg$GraphicSVG$red,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 200, 45)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(0, 60),
				$author$project$SinCreator$copiable('[FROM LEFT TO RIGHT]')),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(0, 50),
				$author$project$SinCreator$copiable('1. The first set of arrows change the wave\'s amplitude.')),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(0, 40),
				$author$project$SinCreator$copiable('2. The second set of arrows change the wave\'s frequency.')),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(0, 30),
				$author$project$SinCreator$copiable('3. The third set of arrows change the wave\'s phase shift.'))
			]));
	var cosLabel = A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
		$author$project$SinCreator$TransM(
			function (m) {
				return _Utils_update(
					m,
					{aD: 1});
			}),
		A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-110, -82),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
				$elm$core$Basics$degrees(90),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						8,
						$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
							$MacCASOutreach$graphicsvg$GraphicSVG$text(
								$elm$core$String$fromFloat(model.b) + ('* cos(' + $author$project$SinCreator$cosinString(model)))))))));
	var circleGraphics = $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
				0.25,
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, model.ba, model.aX, model.aR),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$line,
						_Utils_Tuple2(-50, 50),
						_Utils_Tuple2(
							(-50) + (model.b * notTrigCycleU(uArg)),
							50 + u)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
				0.5,
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, model.ba, model.aX, model.aR),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$line,
						_Utils_Tuple2(
							(-50) + (model.b * notTrigCycleU(uArg)),
							50 + u),
						_Utils_Tuple2(-20, 50 + model.aF)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-20, 50 + model.aF),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, model.ba, model.aX, model.aR),
					$MacCASOutreach$graphicsvg$GraphicSVG$circle(2))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-50, 50),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$black,
					$MacCASOutreach$graphicsvg$GraphicSVG$circle(
						$elm$core$Basics$abs(uScale))))
			]));
	return _List_fromArray(
		[
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
			0.25,
			A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$graphPaperCustom,
				10,
				1,
				A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 255, 137, 5))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-110, -140),
			$MacCASOutreach$graphicsvg$GraphicSVG$group(
				_List_fromArray(
					[
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(-20, 50),
						$MacCASOutreach$graphicsvg$GraphicSVG$group(
							$author$project$SinCreator$sinCurve(model))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(-185, 70),
						$author$project$SinCreator$trigGraphAxis(model)),
						circleGraphics
					]))),
			A2($MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent, 1, titlesText),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-120, 150),
			waveHeader),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-140, -65),
			transformsGraphicsGroup),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(170, -240),
			$MacCASOutreach$graphicsvg$GraphicSVG$group(
				_List_fromArray(
					[
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(5, 150),
						$author$project$SinCreator$functionText(model)),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(0, 165),
						setofTriangles),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
						0.25,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(-40, 150),
							A3(
								$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
								$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
								$MacCASOutreach$graphicsvg$GraphicSVG$red,
								A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 180, 60))))
					]))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(30, 40),
			yourCodeGroup),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(30, -73),
			instructBox)
		]);
};
var $author$project$TextCreator$BlueDown = 3;
var $author$project$TextCreator$BlueUp = 2;
var $author$project$TextCreator$ButtonDown = function (a) {
	return {$: 11, a: a};
};
var $author$project$TextCreator$GreenDown = 5;
var $author$project$TextCreator$GreenUp = 4;
var $author$project$TextCreator$RedDown = 1;
var $author$project$TextCreator$RedUp = 0;
var $author$project$TextCreator$TransM = function (a) {
	return {$: 6, a: a};
};
var $author$project$TextCreator$colourAmount = 3;
var $author$project$TextCreator$cyan = A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 0, 150, 150);
var $author$project$TextCreator$titleColour = $author$project$TextCreator$cyan;
var $author$project$TextCreator$colours = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, -22),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 130, 75)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-30, 14),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 100, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-75, 11),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$TextCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('3. Change the Colour!')))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(2, 0),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
					_List_fromArray(
						[
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$TextCreator$ButtonDown(7),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$TextCreator$ButtonDown(0),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$TextCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													G: (m.G < 254) ? (m.G + 1) : 255
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-95, -10),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(-30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 255, 10, 10),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$TextCreator$ButtonDown(7),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$TextCreator$ButtonDown(1),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$TextCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													G: (m.G > 1) ? (m.G - 1) : 0
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-84, -9),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 180, 140, 140),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$TextCreator$ButtonDown(7),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$TextCreator$ButtonDown(4),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$TextCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													D: (m.D < 254) ? (m.D + 1) : 255
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-65, -10),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(-30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 10, 255, 10),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$TextCreator$ButtonDown(7),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$TextCreator$ButtonDown(5),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$TextCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													D: (m.D > 1) ? (m.D - 1) : 0
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-54, -9),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 140, 180, 140),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$TextCreator$ButtonDown(7),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$TextCreator$ButtonDown(2),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$TextCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													C: (m.C < 254) ? (m.C + 1) : 255
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-35, -10),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(-30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 10, 10, 255),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$TextCreator$ButtonDown(7),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$TextCreator$ButtonDown(3),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$TextCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													C: (m.C > 1) ? (m.C - 1) : 0
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-24, -9),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 140, 140, 180),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(-90, -30),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$black,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$size,
									10,
									$MacCASOutreach$graphicsvg$GraphicSVG$text('Lighter')))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$TextCreator$TransM(
								function (m) {
									return _Utils_update(
										m,
										{
											C: A3($elm$core$Basics$clamp, 0, 255, m.C + 5),
											D: A3($elm$core$Basics$clamp, 0, 255, m.D + 5),
											G: A3($elm$core$Basics$clamp, 0, 255, m.G + 5)
										});
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-75, -27),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$blank,
									A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 32, 10)))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(-45, -30),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$black,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$size,
									10,
									$MacCASOutreach$graphicsvg$GraphicSVG$text('Darker')))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$TextCreator$TransM(
								function (m) {
									return _Utils_update(
										m,
										{
											C: A3($elm$core$Basics$clamp, 0, 255, m.C - 5),
											D: A3($elm$core$Basics$clamp, 0, 255, m.D - 5),
											G: A3($elm$core$Basics$clamp, 0, 255, m.G - 5)
										});
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-30, -27),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$blank,
									A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 32, 10)))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(-100, -50),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$black,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$size,
									10,
									$MacCASOutreach$graphicsvg$GraphicSVG$text('Colourful')))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$TextCreator$TransM(
								function (m) {
									var _v0 = A3($author$project$ShapeCreateAssets$getBrightest, model.G, model.D, model.C);
									switch (_v0) {
										case 6:
											return m;
										case 0:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - $author$project$TextCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - $author$project$TextCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + ($author$project$TextCreator$colourAmount * 2))
												});
										case 2:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - $author$project$TextCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + ($author$project$TextCreator$colourAmount * 2)),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - $author$project$TextCreator$colourAmount)
												});
										case 1:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + ($author$project$TextCreator$colourAmount * 2)),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - $author$project$TextCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - $author$project$TextCreator$colourAmount)
												});
										case 4:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - ($author$project$TextCreator$colourAmount * 2)),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + $author$project$TextCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + $author$project$TextCreator$colourAmount)
												});
										case 3:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + $author$project$TextCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - ($author$project$TextCreator$colourAmount * 2)),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + $author$project$TextCreator$colourAmount)
												});
										default:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + $author$project$TextCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + $author$project$TextCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - ($author$project$TextCreator$colourAmount * 2))
												});
									}
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-80, -47),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$blank,
									A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 40, 10)))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(-44, -50),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$black,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$size,
									10,
									$MacCASOutreach$graphicsvg$GraphicSVG$text('Colourless')))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$TextCreator$TransM(
								function (m) {
									var _v1 = A3($author$project$ShapeCreateAssets$getBrightest, model.G, model.D, model.C);
									switch (_v1) {
										case 6:
											return m;
										case 0:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + $author$project$TextCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + $author$project$TextCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - ($author$project$TextCreator$colourAmount * 2))
												});
										case 2:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + $author$project$TextCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - ($author$project$TextCreator$colourAmount * 2)),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + $author$project$TextCreator$colourAmount)
												});
										case 1:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - ($author$project$TextCreator$colourAmount * 2)),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + $author$project$TextCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + $author$project$TextCreator$colourAmount)
												});
										case 4:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + ($author$project$TextCreator$colourAmount * 2)),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - $author$project$TextCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - $author$project$TextCreator$colourAmount)
												});
										case 3:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - $author$project$TextCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + ($author$project$TextCreator$colourAmount * 2)),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - $author$project$TextCreator$colourAmount)
												});
										default:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - $author$project$TextCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - $author$project$TextCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + ($author$project$TextCreator$colourAmount * 2))
												});
									}
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-23, -47),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$blank,
									A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 44, 10))))
						])))
			]));
};
var $author$project$TextCreator$Bold = 7;
var $author$project$TextCreator$Centered = 15;
var $author$project$TextCreator$Fixedwidth = 13;
var $author$project$TextCreator$Italic = 8;
var $author$project$TextCreator$Sansserif = 11;
var $author$project$TextCreator$Serif = 12;
var $author$project$TextCreator$Size = 14;
var $author$project$TextCreator$Strikethrough = 10;
var $author$project$TextCreator$Toggle = function (a) {
	return {$: 5, a: a};
};
var $author$project$TextCreator$Underline = 9;
var $author$project$TextCreator$time4 = F5(
	function (model, t, w, h, shape) {
		return function () {
			switch (t) {
				case 0:
					return model.b5;
				case 1:
					return model.a_;
				case 2:
					return model.a$;
				case 3:
					return model.a0;
				case 4:
					return model.a1;
				case 5:
					return model.aZ;
				case 6:
					return model.ae;
				case 14:
					return model.aw;
				case 7:
					return model.aq;
				case 8:
					return model.at;
				case 9:
					return model.ay;
				case 10:
					return model.ax;
				case 11:
					return model.au;
				case 12:
					return model.av;
				case 13:
					return model.as;
				case 15:
					return model.ar;
				default:
					return false;
			}
		}() ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4(
						$MacCASOutreach$graphicsvg$GraphicSVG$rgba,
						0,
						200,
						200,
						0.6 + (0.4 * $elm$core$Basics$sin((5 * model.aO) - 1.5))),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h)),
					shape
				])) : shape;
	});
var $author$project$TextCreator$Outlined = 1;
var $author$project$TextCreator$clrString = F2(
	function (m, clr) {
		return '(rgb ' + ($elm$core$String$fromFloat(m.G) + (' ' + ($elm$core$String$fromFloat(m.D) + (' ' + ($elm$core$String$fromFloat(m.C) + ')')))));
	});
var $author$project$TextCreator$stampString = F2(
	function (m, stamp) {
		if (!stamp) {
			return 'filled ';
		} else {
			return 'outlined ';
		}
	});
var $author$project$TextCreator$styleString = function (m) {
	return '(' + (function () {
		var _v0 = m.br;
		switch (_v0) {
			case 0:
				return 'solid ';
			case 1:
				return 'dotted ';
			case 2:
				return 'dashed ';
			case 3:
				return 'longdash ';
			default:
				return 'dotdash ';
		}
	}() + ($elm$core$String$fromFloat(m.I) + ')'));
};
var $author$project$TextCreator$transformString = F2(
	function (m, t) {
		switch (t) {
			case 0:
				return '|> move (' + ($elm$core$String$fromFloat(m.cv + 2) + (',' + ($elm$core$String$fromFloat(m.bg - 3) + ')')));
			case 1:
				return '|> rotate (degrees ' + ($elm$core$String$fromFloat(m.aI) + ')');
			case 2:
				return '|> scale 1.25';
			case 3:
				return '|> scaleX ' + $elm$core$String$fromFloat(m.aB);
			case 4:
				return '|> scaleY ' + $elm$core$String$fromFloat(m.aC);
			case 5:
				return '|> makeTransparent 0.25';
			case 7:
				return '|> bold';
			case 8:
				return '|> italic';
			case 9:
				return '|> underline';
			case 10:
				return '|> strikethrough';
			case 11:
				return '|> sansserif';
			case 12:
				return '|> serif';
			case 13:
				return '|> fixedwidth';
			case 14:
				return '|> size ' + $elm$core$String$fromFloat(m.an);
			case 6:
				return '-- Add a Shadow!';
			case 15:
				return '|> centered';
			default:
				return '|> ' + (A2($author$project$TextCreator$stampString, m, m.aV) + (((m.aV === 1) ? ($author$project$TextCreator$styleString(m) + ' ') : '') + A2($author$project$TextCreator$clrString, m, m.bj)));
		}
	});
var $author$project$TextCreator$effects = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-35, -37),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 140, 99)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-45, 14),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 95, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-85, 11),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$TextCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('5. Apply Effects!')))))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (ss, y) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-35, y),
								A5(
									$author$project$TextCreator$time4,
									model,
									ss,
									140,
									10,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-68, -2.5),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											$author$project$TextCreator$Toggle(ss),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$black,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$size,
													10,
													$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
														$MacCASOutreach$graphicsvg$GraphicSVG$text(
															A2($author$project$TextCreator$transformString, model, ss)))))))));
						}),
					_List_fromArray(
						[14, 7, 8, 9, 11, 12, 10, 13, 15]),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-10) * x;
						},
						A2($elm$core$List$range, 0, 20))))
			]));
};
var $author$project$TextCreator$ClearOne = {$: 9};
var $author$project$TextCreator$Delete = 6;
var $author$project$TextCreator$Key = function (a) {
	return {$: 8, a: a};
};
var $author$project$TextCreator$Tab = {$: 10};
var $MacCASOutreach$graphicsvg$GraphicSVG$AlignCentred = 1;
var $MacCASOutreach$graphicsvg$GraphicSVG$centered = function (stencil) {
	if (stencil.$ === 7) {
		var _v1 = stencil.a;
		var si = _v1.a;
		var bo = _v1.b;
		var i = _v1.c;
		var u = _v1.d;
		var s = _v1.e;
		var sel = _v1.f;
		var f = _v1.g;
		var c = _v1.h;
		var str = stencil.b;
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$Text,
			A8($MacCASOutreach$graphicsvg$GraphicSVG$Face, si, bo, i, u, s, sel, f, 1),
			str);
	} else {
		var a = stencil;
		return a;
	}
};
var $author$project$TextCreator$keyboard = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(60, -23),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 170, 120)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(60, 37),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 89, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(18, 34),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$TextCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('1. Choose Your Text!')))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$red,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 20, 20)),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(100, -57),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
					$author$project$TextCreator$Tab,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 20, 20)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
				$author$project$TextCreator$Tab,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(93, -60),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$grey,
						$MacCASOutreach$graphicsvg$GraphicSVG$text('Aa')))),
				(model.aL <= 2) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (y, z) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(y, z + 6),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$author$project$TextCreator$cyan,
									A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 20, 25)));
						}),
					A2(
						$elm$core$List$map,
						function (x) {
							return 20 * A2($elm$core$Basics$modBy, 7, x);
						},
						A2($elm$core$List$range, 0, 25)),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-21) * ((x / 7) | 0);
						},
						A2($elm$core$List$range, 0, 25)))) : $MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (y, z) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(y, z + 6),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$author$project$TextCreator$cyan,
									A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 20, 25)));
						}),
					A2(
						$elm$core$List$map,
						function (x) {
							return 20 * A2($elm$core$Basics$modBy, 7, x);
						},
						A2($elm$core$List$range, 0, 20)),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-21) * ((x / 7) | 0);
						},
						A2($elm$core$List$range, 0, 20)))),
				(model.aL === 1) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
				_Utils_ap(
					A4(
						$elm$core$List$map3,
						F3(
							function (ss, y, z) {
								return A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(y, z),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$white,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											18,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text(ss)))));
							}),
						_List_fromArray(
							['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']),
						A2(
							$elm$core$List$map,
							function (x) {
								return 20 * A2($elm$core$Basics$modBy, 7, x);
							},
							A2($elm$core$List$range, 0, 25)),
						A2(
							$elm$core$List$map,
							function (x) {
								return (-20) * ((x / 7) | 0);
							},
							A2($elm$core$List$range, 0, 25))),
					A4(
						$elm$core$List$map3,
						F3(
							function (y, z, ss) {
								return A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$TextCreator$Key(ss),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
										0,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(y, z + 6),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$white,
												A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 20, 25)))));
							}),
						A2(
							$elm$core$List$map,
							function (x) {
								return 20 * A2($elm$core$Basics$modBy, 7, x);
							},
							A2($elm$core$List$range, 0, 25)),
						A2(
							$elm$core$List$map,
							function (x) {
								return (-21) * ((x / 7) | 0);
							},
							A2($elm$core$List$range, 0, 25)),
						_List_fromArray(
							['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'])))) : ((model.aL === 2) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
				_Utils_ap(
					A4(
						$elm$core$List$map3,
						F3(
							function (ss, y, z) {
								return A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(y, z),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$white,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											18,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text(ss)))));
							}),
						_List_fromArray(
							['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']),
						A2(
							$elm$core$List$map,
							function (x) {
								return 20 * A2($elm$core$Basics$modBy, 7, x);
							},
							A2($elm$core$List$range, 0, 25)),
						A2(
							$elm$core$List$map,
							function (x) {
								return (-20) * ((x / 7) | 0);
							},
							A2($elm$core$List$range, 0, 25))),
					A4(
						$elm$core$List$map3,
						F3(
							function (y, z, ss) {
								return A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$TextCreator$Key(ss),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
										0,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(y, z + 6),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$white,
												A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 20, 25)))));
							}),
						A2(
							$elm$core$List$map,
							function (x) {
								return 20 * A2($elm$core$Basics$modBy, 7, x);
							},
							A2($elm$core$List$range, 0, 25)),
						A2(
							$elm$core$List$map,
							function (x) {
								return (-21) * ((x / 7) | 0);
							},
							A2($elm$core$List$range, 0, 25)),
						_List_fromArray(
							['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'])))) : ((model.aL === 3) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
				_Utils_ap(
					A4(
						$elm$core$List$map3,
						F3(
							function (ss, y, z) {
								return A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(y, z),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$white,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											18,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text(ss)))));
							}),
						_List_fromArray(
							['`', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', '[', ']', '\"', ';', '\'', ',', '.', '/']),
						A2(
							$elm$core$List$map,
							function (x) {
								return 20 * A2($elm$core$Basics$modBy, 7, x);
							},
							A2($elm$core$List$range, 0, 25)),
						A2(
							$elm$core$List$map,
							function (x) {
								return (-20) * ((x / 7) | 0);
							},
							A2($elm$core$List$range, 0, 25))),
					_Utils_ap(
						A4(
							$elm$core$List$map3,
							F3(
								function (y, z, ss) {
									return A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
										$author$project$TextCreator$Key(ss),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
											0,
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$move,
												_Utils_Tuple2(y, z + 6),
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$filled,
													$MacCASOutreach$graphicsvg$GraphicSVG$white,
													A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 20, 25)))));
								}),
							A2(
								$elm$core$List$map,
								function (x) {
									return 20 * A2($elm$core$Basics$modBy, 7, x);
								},
								A2($elm$core$List$range, 0, 20)),
							A2(
								$elm$core$List$map,
								function (x) {
									return (-21) * ((x / 7) | 0);
								},
								A2($elm$core$List$range, 0, 20)),
							_List_fromArray(
								['`', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', '[', ']', '\"', ';', '\'', ',', '.', '/'])),
						_List_fromArray(
							[
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(40, ((-21) * ((21 / 7) | 0)) + 6),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$author$project$TextCreator$cyan,
									A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 100, 25))),
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(20, (-21) * ((21 / 7) | 0)),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$white,
									$MacCASOutreach$graphicsvg$GraphicSVG$text('SPACE'))),
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$TextCreator$Key(' '),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(40, ((-21) * ((21 / 7) | 0)) + 6),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
										0,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$filled,
											$MacCASOutreach$graphicsvg$GraphicSVG$white,
											A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 100, 25)))))
							])))) : $MacCASOutreach$graphicsvg$GraphicSVG$group(
				_Utils_ap(
					A4(
						$elm$core$List$map3,
						F3(
							function (ss, y, z) {
								return A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(y, z),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$white,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$size,
											18,
											$MacCASOutreach$graphicsvg$GraphicSVG$centered(
												$MacCASOutreach$graphicsvg$GraphicSVG$text(ss)))));
							}),
						_List_fromArray(
							['~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', '{', '}', '|', '', ':', '<', '>', '?']),
						A2(
							$elm$core$List$map,
							function (x) {
								return 20 * A2($elm$core$Basics$modBy, 7, x);
							},
							A2($elm$core$List$range, 0, 25)),
						A2(
							$elm$core$List$map,
							function (x) {
								return (-20) * ((x / 7) | 0);
							},
							A2($elm$core$List$range, 0, 25))),
					A4(
						$elm$core$List$map3,
						F3(
							function (y, z, ss) {
								return A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$TextCreator$Key(ss),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
										0,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(y, z + 6),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$white,
												A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 20, 25)))));
							}),
						A2(
							$elm$core$List$map,
							function (x) {
								return 20 * A2($elm$core$Basics$modBy, 7, x);
							},
							A2($elm$core$List$range, 0, 20)),
						A2(
							$elm$core$List$map,
							function (x) {
								return (-21) * ((x / 7) | 0);
							},
							A2($elm$core$List$range, 0, 20)),
						_List_fromArray(
							['~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', '{', '}', '|', '', ':', '<', '>', '?'])))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
				$author$project$TextCreator$ButtonDown(7),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
					$author$project$TextCreator$ButtonDown(6),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(120, -57),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$TextCreator$ClearOne,
							$MacCASOutreach$graphicsvg$GraphicSVG$group(
								_List_fromArray(
									[
										A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$MacCASOutreach$graphicsvg$GraphicSVG$red,
										$MacCASOutreach$graphicsvg$GraphicSVG$circle(10)),
										A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
										$elm$core$Basics$degrees(45),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$filled,
											$MacCASOutreach$graphicsvg$GraphicSVG$white,
											A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 5, 15))),
										A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
										$elm$core$Basics$degrees(-45),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$filled,
											$MacCASOutreach$graphicsvg$GraphicSVG$white,
											A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 5, 15)))
									]))))))
			]));
};
var $author$project$TextCreator$colourFun = function (m) {
	var _v0 = m.bj;
	return A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, m.G, m.D, m.C);
};
var $author$project$TextCreator$lineStyleFun = function (m) {
	var _v0 = m.br;
	switch (_v0) {
		case 0:
			return $MacCASOutreach$graphicsvg$GraphicSVG$solid(m.I);
		case 1:
			return $MacCASOutreach$graphicsvg$GraphicSVG$dotted(m.I);
		case 2:
			return $MacCASOutreach$graphicsvg$GraphicSVG$dashed(m.I);
		case 3:
			return $MacCASOutreach$graphicsvg$GraphicSVG$longdash(m.I);
		default:
			return $MacCASOutreach$graphicsvg$GraphicSVG$dotdash(m.I);
	}
};
var $MacCASOutreach$graphicsvg$GraphicSVG$Sansserif = {$: 1};
var $MacCASOutreach$graphicsvg$GraphicSVG$sansserif = function (stencil) {
	if (stencil.$ === 7) {
		var _v1 = stencil.a;
		var si = _v1.a;
		var bo = _v1.b;
		var i = _v1.c;
		var u = _v1.d;
		var s = _v1.e;
		var sel = _v1.f;
		var f = _v1.g;
		var c = _v1.h;
		var str = stencil.b;
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$Text,
			A8($MacCASOutreach$graphicsvg$GraphicSVG$Face, si, bo, i, u, s, sel, $MacCASOutreach$graphicsvg$GraphicSVG$Sansserif, c),
			str);
	} else {
		var a = stencil;
		return a;
	}
};
var $MacCASOutreach$graphicsvg$GraphicSVG$strikethrough = function (stencil) {
	if (stencil.$ === 7) {
		var _v1 = stencil.a;
		var si = _v1.a;
		var bo = _v1.b;
		var i = _v1.c;
		var u = _v1.d;
		var s = _v1.e;
		var sel = _v1.f;
		var f = _v1.g;
		var c = _v1.h;
		var str = stencil.b;
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$Text,
			A8($MacCASOutreach$graphicsvg$GraphicSVG$Face, si, bo, i, u, true, sel, f, c),
			str);
	} else {
		var a = stencil;
		return a;
	}
};
var $author$project$TextCreator$shapeFun = function (m) {
	return (m.aZ ? $MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent(m.ad) : function (x) {
		return x;
	})(
		(m.a1 ? $MacCASOutreach$graphicsvg$GraphicSVG$scaleY(m._) : function (x) {
			return x;
		})(
			(m.a0 ? $MacCASOutreach$graphicsvg$GraphicSVG$scaleX(m._) : function (x) {
				return x;
			})(
				(m.a$ ? $MacCASOutreach$graphicsvg$GraphicSVG$scale(m._) : function (x) {
					return x;
				})(
					(m.a_ ? $MacCASOutreach$graphicsvg$GraphicSVG$rotate(
						$elm$core$Basics$degrees(m.aI)) : function (x) {
						return x;
					})(
						(m.b5 ? $MacCASOutreach$graphicsvg$GraphicSVG$move(
							_Utils_Tuple2(m.cv, m.bg)) : function (x) {
							return x;
						})(
							((!m.aV) ? $MacCASOutreach$graphicsvg$GraphicSVG$filled(
								$author$project$TextCreator$colourFun(m)) : A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
								$author$project$TextCreator$lineStyleFun(m),
								$author$project$TextCreator$colourFun(m)))(
								(m.as ? $MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth : function (x) {
									return x;
								})(
									(m.av ? $MacCASOutreach$graphicsvg$GraphicSVG$serif : function (x) {
										return x;
									})(
										(m.au ? $MacCASOutreach$graphicsvg$GraphicSVG$sansserif : function (x) {
											return x;
										})(
											(m.ax ? $MacCASOutreach$graphicsvg$GraphicSVG$strikethrough : function (x) {
												return x;
											})(
												(m.ar ? $MacCASOutreach$graphicsvg$GraphicSVG$centered : function (x) {
													return x;
												})(
													(m.ay ? $MacCASOutreach$graphicsvg$GraphicSVG$underline : function (x) {
														return x;
													})(
														(m.aq ? $MacCASOutreach$graphicsvg$GraphicSVG$bold : function (x) {
															return x;
														})(
															(m.at ? $MacCASOutreach$graphicsvg$GraphicSVG$italic : function (x) {
																return x;
															})(
																(m.aw ? $MacCASOutreach$graphicsvg$GraphicSVG$size(m.an) : function (x) {
																	return x;
																})(
																	$MacCASOutreach$graphicsvg$GraphicSVG$text(m.aP)))))))))))))))));
};
var $author$project$TextCreator$shadow = function (m) {
	return m.ae ? A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
		0.25,
		A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(m.cv + 1, m.bg - 2),
			$author$project$TextCreator$shapeFun(m))) : A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
		0,
		A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$filled,
			$MacCASOutreach$graphicsvg$GraphicSVG$white,
			A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 20, 20)));
};
var $author$project$TextCreator$Draw = function (a) {
	return {$: 2, a: a};
};
var $author$project$TextCreator$LStyle = {$: 3};
var $author$project$TextCreator$time2 = F5(
	function (model, ss, w, h, shape) {
		return _Utils_eq(ss, model.aV) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4(
						$MacCASOutreach$graphicsvg$GraphicSVG$rgba,
						0,
						200,
						200,
						0.6 + (0.4 * $elm$core$Basics$sin((5 * model.aO) - 0.5))),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h)),
					shape
				])) : shape;
	});
var $author$project$TextCreator$stamps = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, -1),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 130, 30)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, 14),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 95, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-85, 11),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$TextCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('2. Fill it or Outline it!')))))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (ss, y) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-40, y),
								A5(
									$author$project$TextCreator$time2,
									model,
									ss,
									130,
									10,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-63, -2.5),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											$author$project$TextCreator$Draw(ss),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$black,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$size,
													10,
													$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
														$MacCASOutreach$graphicsvg$GraphicSVG$text(
															A2($author$project$TextCreator$stampString, model, ss)))))))));
						}),
					_List_fromArray(
						[0, 1]),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-10) * x;
						},
						A2($elm$core$List$range, 0, 20)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
				$author$project$TextCreator$LStyle,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-43, -12.5),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$black,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$size,
							10,
							$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
								$MacCASOutreach$graphicsvg$GraphicSVG$text(
									$author$project$TextCreator$styleString(model)))))))
			]));
};
var $author$project$TextCreator$tweaks = $MacCASOutreach$graphicsvg$GraphicSVG$group(
	_List_fromArray(
		[
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-26, -6),
			A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
				$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
				$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 157, 40)))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-60, 14),
			A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
				$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
				$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$MacCASOutreach$graphicsvg$GraphicSVG$white,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 55, 12)))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-85, 11),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$author$project$TextCreator$titleColour,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					10,
					$MacCASOutreach$graphicsvg$GraphicSVG$italic(
						$MacCASOutreach$graphicsvg$GraphicSVG$serif(
							$MacCASOutreach$graphicsvg$GraphicSVG$text('4. Tweak it!')))))),
			$MacCASOutreach$graphicsvg$GraphicSVG$group(
			A3(
				$elm$core$List$map2,
				F2(
					function (_v0, _v1) {
						var str = _v0.a;
						var msg = _v0.b;
						var x = _v1.a;
						var y = _v1.b;
						return A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2((-68) + x, (-2.5) + y),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								msg,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$black,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$size,
										10,
										$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
											$MacCASOutreach$graphicsvg$GraphicSVG$text(str))))));
					}),
				_List_fromArray(
					[
						_Utils_Tuple2(
						'thicker',
						$author$project$TextCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										I: (m.I < 10) ? (m.I + 0.5) : 10
									});
							})),
						_Utils_Tuple2(
						'thinner',
						$author$project$TextCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										I: (m.I > 0.5) ? (m.I - 0.5) : 0.5
									});
							})),
						_Utils_Tuple2(
						'bigger',
						$author$project$TextCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										an: (m.an < 40) ? (m.an + 2) : 8
									});
							})),
						_Utils_Tuple2(
						'smaller',
						$author$project$TextCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{
										an: (m.an > 8) ? (m.an - 2) : 40
									});
							})),
						_Utils_Tuple2(
						'Add Shadow',
						$author$project$TextCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{ae: true});
							})),
						_Utils_Tuple2(
						'Remove Shadow',
						$author$project$TextCreator$TransM(
							function (m) {
								return _Utils_update(
									m,
									{ae: false});
							}))
					]),
				$elm$core$List$concat(
					A2(
						$elm$core$List$map,
						function (idx) {
							return _List_fromArray(
								[
									_Utils_Tuple2(-30, (-10) * idx),
									_Utils_Tuple2(40, (-10) * idx)
								]);
						},
						A2($elm$core$List$range, 0, 20)))))
		]));
var $author$project$TextCreator$MakeText = 16;
var $author$project$TextCreator$Scale = 2;
var $author$project$TextCreator$code = function (str) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$filled,
		$MacCASOutreach$graphicsvg$GraphicSVG$black,
		A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$size,
			9,
			$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
				$MacCASOutreach$graphicsvg$GraphicSVG$text(str))));
};
var $author$project$TextCreator$codeForShadow = $MacCASOutreach$graphicsvg$GraphicSVG$group(
	_List_fromArray(
		[
			$author$project$TextCreator$code('group [ myStr '),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(0, -10),
			$author$project$TextCreator$code('      , myStr |> move (1,-2)')),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(0, -20),
			$author$project$TextCreator$code('              |> makeTransparent 0.25')),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(0, -30),
			$author$project$TextCreator$code('      ]'))
		]));
var $author$project$TextCreator$howDown = function (m) {
	var add = function (condition) {
		return condition ? 1 : 0;
	};
	return (((((((add(m.aw) + add(m.aq)) + add(m.at)) + add(m.ay)) + add(m.au)) + add(m.av)) + add(m.ax)) + add(m.as)) + add(m.ar);
};
var $author$project$TextCreator$stencilString = F2(
	function (m, shape) {
		switch (shape) {
			case 0:
				return 'circle ' + $elm$core$String$fromFloat(m.dl);
			case 7:
				return 'square ' + $elm$core$String$fromFloat(m.dl);
			case 1:
				return 'rect ' + ($elm$core$String$fromFloat(m.dl) + (' ' + $elm$core$String$fromFloat(m.cS)));
			case 2:
				return 'roundedRect ' + ($elm$core$String$fromFloat(m.dl) + (' ' + ($elm$core$String$fromFloat(m.cS) + (' ' + $elm$core$String$fromFloat(m.c9)))));
			case 3:
				return 'oval ' + ($elm$core$String$fromFloat(m.dl) + (' ' + $elm$core$String$fromFloat(m.cS)));
			case 4:
				return 'ngon ' + ($elm$core$String$fromInt(m.da) + (' ' + $elm$core$String$fromFloat(m.dl)));
			case 5:
				return 'triangle ' + $elm$core$String$fromFloat(m.dl);
			case 6:
				return 'wedge ' + ($elm$core$String$fromFloat(m.dl) + (' ' + $elm$core$String$fromFloat(m.c_)));
			case 9:
				return 'curve (0,0) [Pull (10,0) (20,-10)]';
			case 10:
				return 'polygon [(0,0),(0,-10),(30,0)]';
			case 11:
				return 'openPolygon [(0,0),(0,-10),(30,0)]';
			default:
				return 'text \"' + (m.aP + '\"');
		}
	});
var $author$project$TextCreator$yourCode = function (m) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$move,
		_Utils_Tuple2(0, 70),
		$MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(15, -75),
					A3(
						$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
						$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
						$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
							A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 250, 180)))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-55, 14),
					A3(
						$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
						$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
						$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$white,
							A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 12)))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-90, 11),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$author$project$TextCreator$titleColour,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$size,
							10,
							$MacCASOutreach$graphicsvg$GraphicSVG$italic(
								$MacCASOutreach$graphicsvg$GraphicSVG$serif(
									$MacCASOutreach$graphicsvg$GraphicSVG$text('6. Your code!')))))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-105, 0),
					$author$project$ShapeCreateAssets$copiable('-- Add this new definition to your code')),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-105, -10),
					$author$project$ShapeCreateAssets$copiable('myStr =')),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-45, -10),
					$MacCASOutreach$graphicsvg$GraphicSVG$group(
						_Utils_ap(
							_List_fromArray(
								[
									A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									_Utils_Tuple2(-10, 0),
									$author$project$ShapeCreateAssets$copiable(
										A2($author$project$TextCreator$stencilString, m, m.bT)))
								]),
							A3(
								$elm$core$List$map2,
								F2(
									function (str, y) {
										return A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(0, y),
											$author$project$ShapeCreateAssets$copiable(str));
									}),
								$elm$core$List$concat(
									A2(
										$elm$core$List$map,
										function (_v0) {
											var flag = _v0.a;
											var t = _v0.b;
											return flag ? _List_fromArray(
												[
													'' + A2($author$project$TextCreator$transformString, m, t)
												]) : _List_Nil;
										},
										_List_fromArray(
											[
												_Utils_Tuple2(m.a$, 2),
												_Utils_Tuple2(m.aw, 14),
												_Utils_Tuple2(m.aq, 7),
												_Utils_Tuple2(m.at, 8),
												_Utils_Tuple2(m.ay, 9),
												_Utils_Tuple2(m.ax, 10),
												_Utils_Tuple2(m.au, 11),
												_Utils_Tuple2(m.av, 12),
												_Utils_Tuple2(m.as, 13),
												_Utils_Tuple2(m.ar, 15),
												_Utils_Tuple2(true, 16)
											]))),
								_List_fromArray(
									[-10, -20, -30, -40, -50, -60, -70, -80, -90, -100, -110, -120]))))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(
						-105,
						(-30) - ($author$project$TextCreator$howDown(m) * 10)),
					$author$project$ShapeCreateAssets$copiable('-- Add the following code to your shapes:')),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(20, 0),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						_Utils_Tuple2(
							-105,
							(-40) - ($author$project$TextCreator$howDown(m) * 10)),
						m.ae ? $author$project$TextCreator$codeForShadow : $author$project$ShapeCreateAssets$copiable('myStr')))
				])));
};
var $author$project$TextCreator$view = function (model) {
	return _List_fromArray(
		[
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
			0.5,
			A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$graphPaperCustom,
				10,
				1,
				A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 50, 250, 130))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$filled,
			$author$project$TextCreator$titleColour,
			A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 512, 0.5)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$filled,
			$author$project$TextCreator$titleColour,
			A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 512)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(0, 100),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 4, 0.5))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(3, 100),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 0, 150, 100),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(0,100)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(0, -100),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 4, 0.5))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(3, -100),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 0, 150, 100),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(0,-100)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-100, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-100, 3),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 0, 150, 100),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(-100,0)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(100, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(100, 3),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 0, 150, 100),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(100,0)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-200, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-200, 3),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 0, 150, 100),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(-200,0)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(200, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$darkBlue,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(200, 3),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 0, 150, 100),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(200,0)')))),
			$author$project$TextCreator$shapeFun(model),
			$author$project$TextCreator$shadow(model),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-230, 146),
			$author$project$TextCreator$keyboard(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(55, 169),
			$author$project$TextCreator$stamps(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(230, 169),
			$author$project$TextCreator$colours(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-150, -93),
			$author$project$TextCreator$effects(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-150, -23),
			$author$project$TextCreator$tweaks),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(115, -94),
			$author$project$TextCreator$yourCode(model))
		]);
};
var $author$project$TriCreator$AngleDown = 35;
var $author$project$TriCreator$AngleUp = 34;
var $author$project$TriCreator$ButtonDown = function (a) {
	return {$: 8, a: a};
};
var $author$project$TriCreator$MouseUp = {$: 9};
var $author$project$TriCreator$TransM = function (a) {
	return {$: 6, a: a};
};
var $elm$core$Basics$atan2 = _Basics_atan2;
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $author$project$ShapeCreateAssets$arrow = F4(
	function (start, end, colour, thickness) {
		var length = $elm$core$Basics$sqrt(
			A2($elm$core$Basics$pow, end.a - start.a, 2) + A2($elm$core$Basics$pow, end.b - start.b, 2));
		var angle = function () {
			var _v0 = !(end.a - start.a);
			if (_v0) {
				var _v1 = end.b > 0;
				if (_v1) {
					return $elm$core$Basics$degrees(90);
				} else {
					return $elm$core$Basics$degrees(270);
				}
			} else {
				return A2($elm$core$Basics$atan2, end.b - start.b, end.a - start.a);
			}
		}();
		return $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(thickness),
					colour,
					A2($MacCASOutreach$graphicsvg$GraphicSVG$line, start, end)),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
					angle,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						end,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							colour,
							$MacCASOutreach$graphicsvg$GraphicSVG$triangle(thickness * 2))))
				]));
	});
var $author$project$TriCreator$narrowAngle = $MacCASOutreach$graphicsvg$GraphicSVG$group(
	_List_fromArray(
		[
			A3(
			$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
			$MacCASOutreach$graphicsvg$GraphicSVG$solid(2),
			$MacCASOutreach$graphicsvg$GraphicSVG$black,
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$line,
				_Utils_Tuple2(0, 0),
				_Utils_Tuple2(20, 0))),
			A3(
			$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
			$MacCASOutreach$graphicsvg$GraphicSVG$solid(2),
			$MacCASOutreach$graphicsvg$GraphicSVG$black,
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$line,
				_Utils_Tuple2(0, 0),
				_Utils_Tuple2(13, 13))),
			A4(
			$author$project$ShapeCreateAssets$arrow,
			_Utils_Tuple2(10, 10),
			_Utils_Tuple2(16, 4),
			$MacCASOutreach$graphicsvg$GraphicSVG$red,
			2)
		]));
var $author$project$TriCreator$titleColour = A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 150, 20, 170);
var $author$project$TriCreator$wideAngle = $MacCASOutreach$graphicsvg$GraphicSVG$group(
	_List_fromArray(
		[
			A4(
			$author$project$ShapeCreateAssets$arrow,
			_Utils_Tuple2(10, 5),
			_Utils_Tuple2(3, 11),
			$MacCASOutreach$graphicsvg$GraphicSVG$green,
			2),
			A3(
			$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
			$MacCASOutreach$graphicsvg$GraphicSVG$solid(2),
			$MacCASOutreach$graphicsvg$GraphicSVG$black,
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$line,
				_Utils_Tuple2(0, 0),
				_Utils_Tuple2(20, 0))),
			A3(
			$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
			$MacCASOutreach$graphicsvg$GraphicSVG$solid(2),
			$MacCASOutreach$graphicsvg$GraphicSVG$black,
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$line,
				_Utils_Tuple2(0, 0),
				_Utils_Tuple2(16, 8)))
		]));
var $author$project$TriCreator$angles = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$author$project$TriCreator$titleColour,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					10,
					$MacCASOutreach$graphicsvg$GraphicSVG$italic(
						$MacCASOutreach$graphicsvg$GraphicSVG$serif(
							$MacCASOutreach$graphicsvg$GraphicSVG$text('7. Change the Angle!'))))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				_List_fromArray(
					[
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$move,
						function () {
							var _v0 = model.bT;
							if (_v0 === 4) {
								var _v1 = model.b5;
								if (_v1) {
									return _Utils_Tuple2(model.cv - 175, model.bg + 60);
								} else {
									return _Utils_Tuple2(-175, 60);
								}
							} else {
								return _Utils_Tuple2(0, -20);
							}
						}(),
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$filled,
							$MacCASOutreach$graphicsvg$GraphicSVG$black,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$size,
								10,
								$MacCASOutreach$graphicsvg$GraphicSVG$serif(
									$MacCASOutreach$graphicsvg$GraphicSVG$text('Angle'))))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
						$author$project$TriCreator$MouseUp,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
							$author$project$TriCreator$ButtonDown(34),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$TriCreator$TransM(
									function (m) {
										return _Utils_update(
											m,
											{
												S: A3(
													$elm$core$Basics$clamp,
													$elm$core$Basics$degrees(-180),
													$elm$core$Basics$degrees(180),
													m.S + $elm$core$Basics$degrees(5))
											});
									}),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									function () {
										var _v2 = model.bT;
										if (_v2 === 4) {
											var _v3 = model.b5;
											if (_v3) {
												return _Utils_Tuple2(model.cv - 170, model.bg + 75);
											} else {
												return _Utils_Tuple2(-170, 75);
											}
										} else {
											return _Utils_Tuple2(42, -19);
										}
									}(),
									$MacCASOutreach$graphicsvg$GraphicSVG$group(
										_List_fromArray(
											[
												A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
												0.7,
												A2($MacCASOutreach$graphicsvg$GraphicSVG$scale, 0.8, $author$project$TriCreator$wideAngle)),
												A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$move,
												_Utils_Tuple2(9.5, 5),
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
													0.3,
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$filled,
														$MacCASOutreach$graphicsvg$GraphicSVG$blank,
														A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 22, 18))))
											])))))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
						$author$project$TriCreator$MouseUp,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
							$author$project$TriCreator$ButtonDown(35),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$TriCreator$TransM(
									function (m) {
										return _Utils_update(
											m,
											{
												S: A3(
													$elm$core$Basics$clamp,
													$elm$core$Basics$degrees(-180),
													$elm$core$Basics$degrees(180),
													m.S - $elm$core$Basics$degrees(5))
											});
									}),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$move,
									function () {
										var _v4 = model.bT;
										if (_v4 === 4) {
											var _v5 = model.b5;
											if (_v5) {
												return _Utils_Tuple2(model.cv - 150, model.bg + 50);
											} else {
												return _Utils_Tuple2(-150, 50);
											}
										} else {
											return _Utils_Tuple2(72, -19);
										}
									}(),
									$MacCASOutreach$graphicsvg$GraphicSVG$group(
										_List_fromArray(
											[
												A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
												0.7,
												A2($MacCASOutreach$graphicsvg$GraphicSVG$scale, 0.8, $author$project$TriCreator$narrowAngle)),
												A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$move,
												_Utils_Tuple2(9.5, 5),
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$filled,
													$MacCASOutreach$graphicsvg$GraphicSVG$blank,
													A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 22, 18)))
											]))))))
					]))
			]));
};
var $author$project$TriCreator$code = function (str) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$filled,
		$MacCASOutreach$graphicsvg$GraphicSVG$black,
		A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$size,
			10,
			$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
				$MacCASOutreach$graphicsvg$GraphicSVG$text(str))));
};
var $author$project$TriCreator$BlueDown = 19;
var $author$project$TriCreator$BlueUp = 18;
var $author$project$TriCreator$GreenDown = 21;
var $author$project$TriCreator$GreenUp = 20;
var $author$project$TriCreator$RedDown = 17;
var $author$project$TriCreator$RedUp = 16;
var $author$project$TriCreator$SetColour = function (a) {
	return {$: 4, a: a};
};
var $author$project$TriCreator$clrString = F2(
	function (m, clr) {
		return '(rgb ' + ($elm$core$String$fromFloat(m.G) + (' ' + ($elm$core$String$fromFloat(m.D) + (' ' + ($elm$core$String$fromFloat(m.C) + ')')))));
	});
var $author$project$TriCreator$colourAmount = 3;
var $author$project$TriCreator$time3 = F5(
	function (model, ss, w, h, shape) {
		return _Utils_eq(ss, model.bj) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4(
						$MacCASOutreach$graphicsvg$GraphicSVG$rgba,
						230,
						100,
						230,
						0.6 + (0.4 * $elm$core$Basics$sin((5 * model.aO) - 1))),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h)),
					shape
				])) : shape;
	});
var $author$project$TriCreator$colours = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, -170),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 130, 370)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, 14),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 75, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-75, 11),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$TriCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('3. Change the Colour!')))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(2, -8),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
					_List_fromArray(
						[
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$TriCreator$ButtonDown(36),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$TriCreator$ButtonDown(16),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$TriCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													G: (m.G < 254) ? (m.G + 1) : 255
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-95, -10),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(-30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 255, 10, 10),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$TriCreator$ButtonDown(36),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$TriCreator$ButtonDown(17),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$TriCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													G: (m.G > 1) ? (m.G - 1) : 0
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-84, -9),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 180, 140, 140),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$TriCreator$ButtonDown(36),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$TriCreator$ButtonDown(20),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$TriCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													D: (m.D < 254) ? (m.D + 1) : 255
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-65, -10),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(-30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 10, 255, 10),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$TriCreator$ButtonDown(36),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$TriCreator$ButtonDown(21),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$TriCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													D: (m.D > 1) ? (m.D - 1) : 0
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-54, -9),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 140, 180, 140),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$TriCreator$ButtonDown(36),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$TriCreator$ButtonDown(18),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$TriCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													C: (m.C < 254) ? (m.C + 1) : 255
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-35, -10),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(-30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 10, 10, 255),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$TriCreator$ButtonDown(36),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$TriCreator$ButtonDown(19),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									$author$project$TriCreator$TransM(
										function (m) {
											return _Utils_update(
												m,
												{
													C: (m.C > 1) ? (m.C - 1) : 0
												});
										}),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-24, -9),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$rotate,
											$elm$core$Basics$degrees(30),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 140, 140, 180),
												$MacCASOutreach$graphicsvg$GraphicSVG$triangle(8))))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(-90, -30),
							$author$project$TriCreator$code('Lighter')),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$TriCreator$TransM(
								function (m) {
									return _Utils_update(
										m,
										{
											C: A3($elm$core$Basics$clamp, 0, 255, m.C + 5),
											D: A3($elm$core$Basics$clamp, 0, 255, m.D + 5),
											G: A3($elm$core$Basics$clamp, 0, 255, m.G + 5)
										});
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-75, -27),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$blank,
									A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 32, 10)))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(-45, -30),
							$author$project$TriCreator$code('Darker')),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$TriCreator$TransM(
								function (m) {
									return _Utils_update(
										m,
										{
											C: A3($elm$core$Basics$clamp, 0, 255, m.C - 5),
											D: A3($elm$core$Basics$clamp, 0, 255, m.D - 5),
											G: A3($elm$core$Basics$clamp, 0, 255, m.G - 5)
										});
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-30, -27),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$blank,
									A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 32, 10)))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(-100, -50),
							$author$project$TriCreator$code('Colourful')),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$TriCreator$TransM(
								function (m) {
									var _v0 = A3($author$project$ShapeCreateAssets$getBrightest, model.G, model.D, model.C);
									switch (_v0) {
										case 6:
											return m;
										case 0:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - $author$project$TriCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - $author$project$TriCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + ($author$project$TriCreator$colourAmount * 2))
												});
										case 2:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - $author$project$TriCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + ($author$project$TriCreator$colourAmount * 2)),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - $author$project$TriCreator$colourAmount)
												});
										case 1:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + ($author$project$TriCreator$colourAmount * 2)),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - $author$project$TriCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - $author$project$TriCreator$colourAmount)
												});
										case 4:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - ($author$project$TriCreator$colourAmount * 2)),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + $author$project$TriCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + $author$project$TriCreator$colourAmount)
												});
										case 3:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + $author$project$TriCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - ($author$project$TriCreator$colourAmount * 2)),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + $author$project$TriCreator$colourAmount)
												});
										default:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + $author$project$TriCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + $author$project$TriCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - ($author$project$TriCreator$colourAmount * 2))
												});
									}
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-80, -47),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$blank,
									A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 40, 10)))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(-44, -50),
							$author$project$TriCreator$code('Colourless')),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
							$author$project$TriCreator$TransM(
								function (m) {
									var _v1 = A3($author$project$ShapeCreateAssets$getBrightest, model.G, model.D, model.C);
									switch (_v1) {
										case 6:
											return m;
										case 0:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + $author$project$TriCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + $author$project$TriCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - ($author$project$TriCreator$colourAmount * 2))
												});
										case 2:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + $author$project$TriCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - ($author$project$TriCreator$colourAmount * 2)),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + $author$project$TriCreator$colourAmount)
												});
										case 1:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - ($author$project$TriCreator$colourAmount * 2)),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + $author$project$TriCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + $author$project$TriCreator$colourAmount)
												});
										case 4:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C + ($author$project$TriCreator$colourAmount * 2)),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - $author$project$TriCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - $author$project$TriCreator$colourAmount)
												});
										case 3:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - $author$project$TriCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D + ($author$project$TriCreator$colourAmount * 2)),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G - $author$project$TriCreator$colourAmount)
												});
										default:
											return _Utils_update(
												m,
												{
													C: A3($elm$core$Basics$clamp, 0, 255, m.C - $author$project$TriCreator$colourAmount),
													D: A3($elm$core$Basics$clamp, 0, 255, m.D - $author$project$TriCreator$colourAmount),
													G: A3($elm$core$Basics$clamp, 0, 255, m.G + ($author$project$TriCreator$colourAmount * 2))
												});
									}
								}),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-23, -47),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$blank,
									A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 44, 10))))
						]))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (ss, y) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-40, y),
								A5(
									$author$project$TriCreator$time3,
									model,
									ss,
									130,
									10,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-63, -2.5),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											$author$project$TriCreator$SetColour(ss),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$black,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$size,
													10,
													$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
														$MacCASOutreach$graphicsvg$GraphicSVG$text(
															A2($author$project$TriCreator$clrString, model, ss)))))))));
						}),
					_List_fromArray(
						[0]),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-10) * x;
						},
						A2($elm$core$List$range, 0, 40))))
			]));
};
var $author$project$TriCreator$P1Down = 5;
var $author$project$TriCreator$P1Left = 6;
var $author$project$TriCreator$P1Right = 7;
var $author$project$TriCreator$P1Up = 4;
var $author$project$TriCreator$P2Down = 9;
var $author$project$TriCreator$P2Left = 10;
var $author$project$TriCreator$P2Right = 11;
var $author$project$TriCreator$P2Up = 8;
var $author$project$TriCreator$P3Down = 13;
var $author$project$TriCreator$P3Left = 14;
var $author$project$TriCreator$P3Right = 15;
var $author$project$TriCreator$P3Up = 12;
var $author$project$TriCreator$downArrowKey = F3(
	function (tricol, x, y) {
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(x, y),
			A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
				$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
				A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 0, 0, 0, 0.2),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					tricol,
					$MacCASOutreach$graphicsvg$GraphicSVG$polygon(
						_List_fromArray(
							[
								_Utils_Tuple2(-15, 0),
								_Utils_Tuple2(0, -20),
								_Utils_Tuple2(15, 0)
							])))));
	});
var $author$project$TriCreator$leftArrowKey = F3(
	function (tricol, x, y) {
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(x, y),
			A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
				$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
				A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 0, 0, 0, 0.2),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					tricol,
					$MacCASOutreach$graphicsvg$GraphicSVG$polygon(
						_List_fromArray(
							[
								_Utils_Tuple2(0, 15),
								_Utils_Tuple2(-20, 0),
								_Utils_Tuple2(0, -15)
							])))));
	});
var $author$project$TriCreator$rightArrowKey = F3(
	function (tricol, x, y) {
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(x, y),
			A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
				$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
				A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 0, 0, 0, 0.2),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					tricol,
					$MacCASOutreach$graphicsvg$GraphicSVG$polygon(
						_List_fromArray(
							[
								_Utils_Tuple2(0, -15),
								_Utils_Tuple2(20, 0),
								_Utils_Tuple2(0, 15)
							])))));
	});
var $author$project$TriCreator$upArrowKey = F3(
	function (tricol, x, y) {
		return A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(x, y),
			A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
				$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
				A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 0, 0, 0, 0.2),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					tricol,
					$MacCASOutreach$graphicsvg$GraphicSVG$polygon(
						_List_fromArray(
							[
								_Utils_Tuple2(15, 0),
								_Utils_Tuple2(0, 20),
								_Utils_Tuple2(-15, 0)
							])))));
	});
var $author$project$TriCreator$points = function (model) {
	return A2(
		$MacCASOutreach$graphicsvg$GraphicSVG$move,
		_Utils_Tuple2(5, 0),
		$MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-390, 8),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$author$project$TriCreator$titleColour,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$size,
							10,
							$MacCASOutreach$graphicsvg$GraphicSVG$italic(
								$MacCASOutreach$graphicsvg$GraphicSVG$serif(
									$MacCASOutreach$graphicsvg$GraphicSVG$text('6. Move the Points!')))))),
					$MacCASOutreach$graphicsvg$GraphicSVG$group(
					_List_fromArray(
						[
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(-410, -21),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$black,
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$size,
									10,
									$MacCASOutreach$graphicsvg$GraphicSVG$serif(
										$MacCASOutreach$graphicsvg$GraphicSVG$text('Point 1'))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(-375, 37),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$scale,
								0.55,
								$MacCASOutreach$graphicsvg$GraphicSVG$group(
									_List_fromArray(
										[
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
											$author$project$TriCreator$ButtonDown(36),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
												$author$project$TriCreator$ButtonDown(4),
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
													$author$project$TriCreator$TransM(
														function (m) {
															return _Utils_update(
																m,
																{V: m.V + 1});
														}),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
														0.4,
														A3($author$project$TriCreator$upArrowKey, $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple, -40, -80))))),
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
											$author$project$TriCreator$ButtonDown(36),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
												$author$project$TriCreator$ButtonDown(5),
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
													$author$project$TriCreator$TransM(
														function (m) {
															return _Utils_update(
																m,
																{V: m.V - 1});
														}),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
														0.4,
														A3($author$project$TriCreator$downArrowKey, $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple, -40, -120))))),
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
											$author$project$TriCreator$ButtonDown(36),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
												$author$project$TriCreator$ButtonDown(7),
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
													$author$project$TriCreator$TransM(
														function (m) {
															return _Utils_update(
																m,
																{U: m.U + 1});
														}),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
														0.4,
														A3($author$project$TriCreator$rightArrowKey, $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple, -20, -100))))),
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
											$author$project$TriCreator$ButtonDown(36),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
												$author$project$TriCreator$ButtonDown(6),
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
													$author$project$TriCreator$TransM(
														function (m) {
															return _Utils_update(
																m,
																{U: m.U - 1});
														}),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
														0.4,
														A3($author$project$TriCreator$leftArrowKey, $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple, -60, -100)))))
										]))))
						])),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(55, 0),
					$MacCASOutreach$graphicsvg$GraphicSVG$group(
						_List_fromArray(
							[
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-410, -21),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$black,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$size,
										10,
										$MacCASOutreach$graphicsvg$GraphicSVG$serif(
											$MacCASOutreach$graphicsvg$GraphicSVG$text('Point 2'))))),
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-375, 37),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$scale,
									0.55,
									$MacCASOutreach$graphicsvg$GraphicSVG$group(
										_List_fromArray(
											[
												A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
												$author$project$TriCreator$ButtonDown(36),
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
													$author$project$TriCreator$ButtonDown(8),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
														$author$project$TriCreator$TransM(
															function (m) {
																return _Utils_update(
																	m,
																	{Z: m.Z + 1});
															}),
														A2(
															$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
															0.4,
															A3($author$project$TriCreator$upArrowKey, $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple, -40, -80))))),
												A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
												$author$project$TriCreator$ButtonDown(36),
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
													$author$project$TriCreator$ButtonDown(9),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
														$author$project$TriCreator$TransM(
															function (m) {
																return _Utils_update(
																	m,
																	{Z: m.Z - 1});
															}),
														A2(
															$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
															0.4,
															A3($author$project$TriCreator$downArrowKey, $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple, -40, -120))))),
												A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
												$author$project$TriCreator$ButtonDown(36),
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
													$author$project$TriCreator$ButtonDown(11),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
														$author$project$TriCreator$TransM(
															function (m) {
																return _Utils_update(
																	m,
																	{Y: m.Y + 1});
															}),
														A2(
															$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
															0.4,
															A3($author$project$TriCreator$rightArrowKey, $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple, -20, -100))))),
												A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
												$author$project$TriCreator$ButtonDown(36),
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
													$author$project$TriCreator$ButtonDown(10),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
														$author$project$TriCreator$TransM(
															function (m) {
																return _Utils_update(
																	m,
																	{Y: m.Y - 1});
															}),
														A2(
															$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
															0.4,
															A3($author$project$TriCreator$leftArrowKey, $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple, -60, -100)))))
											]))))
							]))),
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(110, 0),
					$MacCASOutreach$graphicsvg$GraphicSVG$group(
						_List_fromArray(
							[
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-410, -21),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$filled,
									$MacCASOutreach$graphicsvg$GraphicSVG$black,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$size,
										10,
										$MacCASOutreach$graphicsvg$GraphicSVG$serif(
											$MacCASOutreach$graphicsvg$GraphicSVG$text('Point 3'))))),
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-375, 37),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$scale,
									0.55,
									$MacCASOutreach$graphicsvg$GraphicSVG$group(
										_List_fromArray(
											[
												A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
												$author$project$TriCreator$ButtonDown(36),
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
													$author$project$TriCreator$ButtonDown(12),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
														$author$project$TriCreator$TransM(
															function (m) {
																return _Utils_update(
																	m,
																	{X: m.X + 1});
															}),
														A2(
															$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
															0.4,
															A3($author$project$TriCreator$upArrowKey, $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple, -40, -80))))),
												A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
												$author$project$TriCreator$ButtonDown(36),
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
													$author$project$TriCreator$ButtonDown(13),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
														$author$project$TriCreator$TransM(
															function (m) {
																return _Utils_update(
																	m,
																	{X: m.X - 1});
															}),
														A2(
															$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
															0.4,
															A3($author$project$TriCreator$downArrowKey, $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple, -40, -120))))),
												A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
												$author$project$TriCreator$ButtonDown(36),
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
													$author$project$TriCreator$ButtonDown(15),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
														$author$project$TriCreator$TransM(
															function (m) {
																return _Utils_update(
																	m,
																	{W: m.W + 1});
															}),
														A2(
															$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
															0.4,
															A3($author$project$TriCreator$rightArrowKey, $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple, -20, -100))))),
												A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
												$author$project$TriCreator$ButtonDown(36),
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
													$author$project$TriCreator$ButtonDown(14),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
														$author$project$TriCreator$TransM(
															function (m) {
																return _Utils_update(
																	m,
																	{W: m.W - 1});
															}),
														A2(
															$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
															0.4,
															A3($author$project$TriCreator$leftArrowKey, $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple, -60, -100)))))
											]))))
							])))
				])));
};
var $author$project$TriCreator$colourFun = function (m) {
	var _v0 = m.bj;
	return A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, m.G, m.D, m.C);
};
var $author$project$TriCreator$lineStyleFun = function (m) {
	var _v0 = m.br;
	switch (_v0) {
		case 0:
			return $MacCASOutreach$graphicsvg$GraphicSVG$solid(m.I);
		case 1:
			return $MacCASOutreach$graphicsvg$GraphicSVG$dotted(m.I);
		case 2:
			return $MacCASOutreach$graphicsvg$GraphicSVG$dashed(m.I);
		case 3:
			return $MacCASOutreach$graphicsvg$GraphicSVG$longdash(m.I);
		default:
			return $MacCASOutreach$graphicsvg$GraphicSVG$dotdash(m.I);
	}
};
var $MacCASOutreach$graphicsvg$GraphicSVG$isosceles = F2(
	function (base, height) {
		return $MacCASOutreach$graphicsvg$GraphicSVG$polygon(
			_List_fromArray(
				[
					_Utils_Tuple2((-base) / 2, 0),
					_Utils_Tuple2(base / 2, 0),
					_Utils_Tuple2(0, height)
				]));
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$sideTwoPoint = F2(
	function (angle, sideOne) {
		return _Utils_Tuple2(
			$elm$core$Basics$cos(angle) * sideOne,
			$elm$core$Basics$sin(angle) * sideOne);
	});
var $MacCASOutreach$graphicsvg$GraphicSVG$sideAngleSide = F3(
	function (sideOne, angle, sideTwo) {
		return $MacCASOutreach$graphicsvg$GraphicSVG$polygon(
			_List_fromArray(
				[
					A2($MacCASOutreach$graphicsvg$GraphicSVG$sideTwoPoint, angle, sideOne),
					_Utils_Tuple2(0, 0),
					_Utils_Tuple2(sideTwo, 0)
				]));
	});
var $author$project$TriCreator$stencilFun = function (m) {
	var _v0 = m.bT;
	switch (_v0) {
		case 1:
			return $MacCASOutreach$graphicsvg$GraphicSVG$triangle(m.dl);
		case 2:
			return A2($MacCASOutreach$graphicsvg$GraphicSVG$isosceles, m.dl, m.cS);
		case 0:
			return A2($MacCASOutreach$graphicsvg$GraphicSVG$rightTriangle, m.dl, m.cS);
		case 4:
			return A3($MacCASOutreach$graphicsvg$GraphicSVG$sideAngleSide, m.cS, m.S, m.dl);
		default:
			return $MacCASOutreach$graphicsvg$GraphicSVG$polygon(
				_List_fromArray(
					[
						_Utils_Tuple2(m.U, m.V),
						_Utils_Tuple2(m.Y, m.Z),
						_Utils_Tuple2(m.W, m.X)
					]));
	}
};
var $author$project$TriCreator$shapeFun = function (m) {
	return (m.aZ ? $MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent(m.ad) : function (x) {
		return x;
	})(
		(m.a1 ? $MacCASOutreach$graphicsvg$GraphicSVG$scaleY(m._) : function (x) {
			return x;
		})(
			(m.a0 ? $MacCASOutreach$graphicsvg$GraphicSVG$scaleX(m._) : function (x) {
				return x;
			})(
				(m.a$ ? $MacCASOutreach$graphicsvg$GraphicSVG$scale(m._) : function (x) {
					return x;
				})(
					(m.a_ ? $MacCASOutreach$graphicsvg$GraphicSVG$rotate(
						$elm$core$Basics$degrees(m.aI)) : function (x) {
						return x;
					})(
						(m.b5 ? $MacCASOutreach$graphicsvg$GraphicSVG$move(
							_Utils_Tuple2(m.cv, m.bg)) : function (x) {
							return x;
						})(
							function () {
								var _v0 = m.aV;
								if (!_v0) {
									return A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$filled,
										$author$project$TriCreator$colourFun(m),
										$author$project$TriCreator$stencilFun(m));
								} else {
									return A3(
										$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
										$author$project$TriCreator$lineStyleFun(m),
										$author$project$TriCreator$colourFun(m),
										$author$project$TriCreator$stencilFun(m));
								}
							}()))))));
};
var $author$project$TriCreator$Draw = function (a) {
	return {$: 2, a: a};
};
var $author$project$TriCreator$LStyle = {$: 3};
var $author$project$TriCreator$Outlined = 1;
var $author$project$TriCreator$stampString = F2(
	function (m, stamp) {
		if (!stamp) {
			return 'filled ';
		} else {
			return 'outlined ';
		}
	});
var $author$project$TriCreator$styleString = function (m) {
	return '(' + (function () {
		var _v0 = m.br;
		switch (_v0) {
			case 0:
				return 'solid ';
			case 1:
				return 'dotted ';
			case 2:
				return 'dashed ';
			case 3:
				return 'longdash ';
			default:
				return 'dotdash ';
		}
	}() + ($elm$core$String$fromFloat(m.I) + ')'));
};
var $author$project$TriCreator$time2 = F5(
	function (model, ss, w, h, shape) {
		return _Utils_eq(ss, model.aV) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4(
						$MacCASOutreach$graphicsvg$GraphicSVG$rgba,
						230,
						100,
						230,
						0.6 + (0.4 * $elm$core$Basics$sin((5 * model.aO) - 0.5))),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h)),
					shape
				])) : shape;
	});
var $author$project$TriCreator$stamps = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, -1),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 130, 30)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-40, 14),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 95, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-85, 11),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$TriCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('2. Fill it or Outline it!')))))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (ss, y) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-40, y),
								A5(
									$author$project$TriCreator$time2,
									model,
									ss,
									130,
									10,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-63, -2.5),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											$author$project$TriCreator$Draw(ss),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$black,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$size,
													10,
													$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
														$MacCASOutreach$graphicsvg$GraphicSVG$text(
															A2($author$project$TriCreator$stampString, model, ss)))))))));
						}),
					_List_fromArray(
						[0, 1]),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-10) * x;
						},
						A2($elm$core$List$range, 0, 20)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
				$author$project$TriCreator$LStyle,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$move,
					_Utils_Tuple2(-43, -12.5),
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$black,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$size,
							10,
							$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
								$MacCASOutreach$graphicsvg$GraphicSVG$text(
									$author$project$TriCreator$styleString(model)))))))
			]));
};
var $author$project$TriCreator$Isosceles = 2;
var $author$project$TriCreator$RightAngle = 0;
var $author$project$TriCreator$SideAngleSide = 4;
var $author$project$TriCreator$Sten = function (a) {
	return {$: 1, a: a};
};
var $author$project$TriCreator$ThreePoint = 3;
var $author$project$TriCreator$toDegrees = function (radianValue) {
	return $elm$core$Basics$round((radianValue * 180) / $elm$core$Basics$pi);
};
var $author$project$TriCreator$stencilString = F2(
	function (m, shape) {
		switch (shape) {
			case 1:
				return 'triangle ' + $elm$core$String$fromFloat(m.dl);
			case 0:
				return 'rightTriangle ' + ($elm$core$String$fromFloat(m.dl) + (' ' + $elm$core$String$fromFloat(m.cS)));
			case 2:
				return 'isosceles ' + ($elm$core$String$fromFloat(m.dl) + (' ' + $elm$core$String$fromFloat(m.cS)));
			case 4:
				return 'sideAngleSide ' + ($elm$core$String$fromFloat(m.cS) + (' (degrees ' + ($elm$core$String$fromInt(
					$author$project$TriCreator$toDegrees(m.S)) + (') ' + $elm$core$String$fromFloat(m.dl)))));
			default:
				return 'polygon [(' + ($elm$core$String$fromFloat(m.U) + (',' + ($elm$core$String$fromFloat(m.V) + ('),(' + ($elm$core$String$fromFloat(m.Y) + (',' + ($elm$core$String$fromFloat(m.Z) + ('),(' + ($elm$core$String$fromFloat(m.W) + (',' + ($elm$core$String$fromFloat(m.X) + ')]')))))))))));
		}
	});
var $author$project$TriCreator$time1 = F5(
	function (model, ss, w, h, shape) {
		return _Utils_eq(ss, model.bT) ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4(
						$MacCASOutreach$graphicsvg$GraphicSVG$rgba,
						230,
						100,
						230,
						0.6 + (0.4 * $elm$core$Basics$sin(5 * model.aO))),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h)),
					shape
				])) : shape;
	});
var $author$project$TriCreator$stencils = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(0, -47),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 210, 120)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(0, 13),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 75, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-35, 10),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$TriCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('1. Pick a Stencil!')))))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (ss, y) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(0, y),
								A5(
									$author$project$TriCreator$time1,
									model,
									ss,
									210,
									10,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-103, -2.5),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											$author$project$TriCreator$Sten(ss),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$black,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$size,
													9,
													$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
														$MacCASOutreach$graphicsvg$GraphicSVG$text(
															A2($author$project$TriCreator$stencilString, model, ss)))))))));
						}),
					_List_fromArray(
						[1, 0, 2, 4, 3]),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-10) * x;
						},
						A2($elm$core$List$range, 0, 20))))
			]));
};
var $author$project$TriCreator$MakeTransparent = 5;
var $author$project$TriCreator$Move = 0;
var $author$project$TriCreator$Rotate = 1;
var $author$project$TriCreator$Scale = 2;
var $author$project$TriCreator$ScaleX = 3;
var $author$project$TriCreator$ScaleY = 4;
var $author$project$TriCreator$Toggle = function (a) {
	return {$: 5, a: a};
};
var $author$project$TriCreator$time4 = F5(
	function (model, t, w, h, shape) {
		return function () {
			switch (t) {
				case 0:
					return model.b5;
				case 1:
					return model.a_;
				case 2:
					return model.a$;
				case 3:
					return model.a0;
				case 4:
					return model.a1;
				default:
					return model.aZ;
			}
		}() ? $MacCASOutreach$graphicsvg$GraphicSVG$group(
			_List_fromArray(
				[
					A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					A4(
						$MacCASOutreach$graphicsvg$GraphicSVG$rgba,
						230,
						100,
						230,
						0.6 + (0.4 * $elm$core$Basics$sin((5 * model.aO) - 1.5))),
					A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, w, h)),
					shape
				])) : shape;
	});
var $author$project$TriCreator$transformString = F2(
	function (m, t) {
		switch (t) {
			case 0:
				return '|> move (' + ($elm$core$String$fromFloat(m.cv) + (',' + ($elm$core$String$fromFloat(m.bg) + ')')));
			case 1:
				return '|> rotate (degrees ' + ($elm$core$String$fromFloat(m.aI) + ')');
			case 2:
				return '|> scale ' + $elm$core$String$fromFloat(m._);
			case 3:
				return '|> scaleX ' + $elm$core$String$fromFloat(m.aB);
			case 4:
				return '|> scaleY ' + $elm$core$String$fromFloat(m.aC);
			default:
				return '|> makeTransparent ' + $elm$core$String$fromFloat(m.ad);
		}
	});
var $author$project$TriCreator$transforms = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-35, -21),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 140, 70)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-45, 14),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 95, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-85, 11),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$TriCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('4. Apply Transforms!')))))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (ss, y) {
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-35, y),
								A5(
									$author$project$TriCreator$time4,
									model,
									ss,
									140,
									10,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(-68, -2.5),
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											$author$project$TriCreator$Toggle(ss),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$black,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$size,
													10,
													$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
														$MacCASOutreach$graphicsvg$GraphicSVG$text(
															A2($author$project$TriCreator$transformString, model, ss)))))))));
						}),
					_List_fromArray(
						[2, 3, 4, 1, 0, 5]),
					A2(
						$elm$core$List$map,
						function (x) {
							return (-10) * x;
						},
						A2($elm$core$List$range, 0, 20))))
			]));
};
var $author$project$TriCreator$Bigger = 30;
var $author$project$TriCreator$Down = 1;
var $author$project$TriCreator$Ghostier = 32;
var $author$project$TriCreator$Left = 2;
var $author$project$TriCreator$Narrower = 27;
var $author$project$TriCreator$Right = 3;
var $author$project$TriCreator$RotateClock = 22;
var $author$project$TriCreator$RotateCounter = 23;
var $author$project$TriCreator$Shorter = 29;
var $author$project$TriCreator$Smaller = 31;
var $author$project$TriCreator$Solider = 33;
var $author$project$TriCreator$Taller = 28;
var $author$project$TriCreator$Thicker = 24;
var $author$project$TriCreator$Thinner = 25;
var $author$project$TriCreator$Up = 0;
var $author$project$TriCreator$Wider = 26;
var $author$project$TriCreator$tweaks = function (model) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-35, -61),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 140, 150)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-60, 14),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 55, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-85, 11),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$TriCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('5. Tweak it!')))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-7, 0),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
					_List_fromArray(
						[
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$TriCreator$ButtonDown(36),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$TriCreator$ButtonDown(0),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									function () {
										var _v1 = model.b5;
										if (_v1) {
											return $author$project$TriCreator$TransM(
												function (m) {
													return _Utils_update(
														m,
														{bg: m.bg + 1});
												});
										} else {
											return $author$project$TriCreator$TransM(
												function (m) {
													return m;
												});
										}
									}(),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
										function () {
											var _v0 = model.b5;
											if (_v0) {
												return 0.6;
											} else {
												return 0.1;
											}
										}(),
										A3($author$project$TriCreator$upArrowKey, $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple, -40, -85))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$TriCreator$ButtonDown(36),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$TriCreator$ButtonDown(1),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									function () {
										var _v3 = model.b5;
										if (_v3) {
											return $author$project$TriCreator$TransM(
												function (m) {
													return _Utils_update(
														m,
														{bg: m.bg - 1});
												});
										} else {
											return $author$project$TriCreator$TransM(
												function (m) {
													return m;
												});
										}
									}(),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
										function () {
											var _v2 = model.b5;
											if (_v2) {
												return 0.6;
											} else {
												return 0.1;
											}
										}(),
										A3($author$project$TriCreator$downArrowKey, $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple, -40, -115))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$TriCreator$ButtonDown(36),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$TriCreator$ButtonDown(3),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									function () {
										var _v5 = model.b5;
										if (_v5) {
											return $author$project$TriCreator$TransM(
												function (m) {
													return _Utils_update(
														m,
														{cv: m.cv + 1});
												});
										} else {
											return $author$project$TriCreator$TransM(
												function (m) {
													return m;
												});
										}
									}(),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
										function () {
											var _v4 = model.b5;
											if (_v4) {
												return 0.6;
											} else {
												return 0.1;
											}
										}(),
										A3($author$project$TriCreator$rightArrowKey, $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple, -25, -100))))),
							A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
							$author$project$TriCreator$ButtonDown(36),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
								$author$project$TriCreator$ButtonDown(2),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
									function () {
										var _v7 = model.b5;
										if (_v7) {
											return $author$project$TriCreator$TransM(
												function (m) {
													return _Utils_update(
														m,
														{cv: m.cv - 1});
												});
										} else {
											return $author$project$TriCreator$TransM(
												function (m) {
													return m;
												});
										}
									}(),
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
										function () {
											var _v6 = model.b5;
											if (_v6) {
												return 0.6;
											} else {
												return 0.1;
											}
										}(),
										A3($author$project$TriCreator$leftArrowKey, $MacCASOutreach$graphicsvg$GraphicSVG$lightPurple, -55, -100)))))
						]))),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
				A3(
					$elm$core$List$map2,
					F2(
						function (_v8, _v10) {
							var str = _v8.a;
							var _v9 = _v8.b;
							var msg = _v9.a;
							var msgd = _v9.b;
							var msgu = _v9.c;
							var x = _v10.a;
							var y = _v10.b;
							return A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2((-68) + x, (-2.5) + y),
								A2(
									$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseUp,
									msgu,
									A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseDown,
										msgd,
										A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
											msg,
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$filled,
												$MacCASOutreach$graphicsvg$GraphicSVG$black,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$size,
													10,
													$MacCASOutreach$graphicsvg$GraphicSVG$fixedwidth(
														$MacCASOutreach$graphicsvg$GraphicSVG$text(str))))))));
						}),
					_List_fromArray(
						[
							_Utils_Tuple2(
							'clockwise',
							_Utils_Tuple3(
								$author$project$TriCreator$TransM(
									function (m) {
										return _Utils_update(
											m,
											{
												aI: $author$project$ShapeCreateAssets$angleBound(m.aI - 1)
											});
									}),
								$author$project$TriCreator$ButtonDown(22),
								$author$project$TriCreator$ButtonDown(36))),
							_Utils_Tuple2(
							'counter',
							_Utils_Tuple3(
								$author$project$TriCreator$TransM(
									function (m) {
										return _Utils_update(
											m,
											{
												aI: $author$project$ShapeCreateAssets$angleBound(m.aI + 1)
											});
									}),
								$author$project$TriCreator$ButtonDown(23),
								$author$project$TriCreator$ButtonDown(36))),
							_Utils_Tuple2(
							'wider',
							_Utils_Tuple3(
								$author$project$TriCreator$TransM(
									function (m) {
										return _Utils_update(
											m,
											{
												dl: (m.dl < 100) ? (m.dl + 1) : 100
											});
									}),
								$author$project$TriCreator$ButtonDown(26),
								$author$project$TriCreator$ButtonDown(36))),
							_Utils_Tuple2(
							'narrower',
							_Utils_Tuple3(
								$author$project$TriCreator$TransM(
									function (m) {
										return _Utils_update(
											m,
											{
												dl: (m.dl > 10) ? (m.dl - 1) : 10
											});
									}),
								$author$project$TriCreator$ButtonDown(27),
								$author$project$TriCreator$ButtonDown(36))),
							_Utils_Tuple2(
							'taller',
							_Utils_Tuple3(
								$author$project$TriCreator$TransM(
									function (m) {
										return _Utils_update(
											m,
											{
												cS: (m.cS < 100) ? (m.cS + 1) : 100
											});
									}),
								$author$project$TriCreator$ButtonDown(28),
								$author$project$TriCreator$ButtonDown(36))),
							_Utils_Tuple2(
							'shorter',
							_Utils_Tuple3(
								$author$project$TriCreator$TransM(
									function (m) {
										return _Utils_update(
											m,
											{
												cS: (m.cS > 10) ? (m.cS - 1) : 10
											});
									}),
								$author$project$TriCreator$ButtonDown(29),
								$author$project$TriCreator$ButtonDown(36))),
							_Utils_Tuple2(
							'thicker',
							_Utils_Tuple3(
								$author$project$TriCreator$TransM(
									function (m) {
										return _Utils_update(
											m,
											{
												I: (m.I < 10) ? (m.I + 0.5) : 10
											});
									}),
								$author$project$TriCreator$ButtonDown(24),
								$author$project$TriCreator$ButtonDown(36))),
							_Utils_Tuple2(
							'thinner',
							_Utils_Tuple3(
								$author$project$TriCreator$TransM(
									function (m) {
										return _Utils_update(
											m,
											{
												I: (m.I > 0.5) ? (m.I - 0.5) : 0.5
											});
									}),
								$author$project$TriCreator$ButtonDown(25),
								$author$project$TriCreator$ButtonDown(36))),
							_Utils_Tuple2(
							'solider',
							_Utils_Tuple3(
								$author$project$TriCreator$TransM(
									function (m) {
										return _Utils_update(
											m,
											{
												ad: (m.ad < 1) ? (m.ad + 0.125) : 1
											});
									}),
								$author$project$TriCreator$ButtonDown(33),
								$author$project$TriCreator$ButtonDown(36))),
							_Utils_Tuple2(
							'ghostier',
							_Utils_Tuple3(
								$author$project$TriCreator$TransM(
									function (m) {
										return _Utils_update(
											m,
											{
												ad: (m.ad > 0) ? (m.ad - 0.125) : 0
											});
									}),
								$author$project$TriCreator$ButtonDown(32),
								$author$project$TriCreator$ButtonDown(36))),
							_Utils_Tuple2(
							'bigger',
							_Utils_Tuple3(
								$author$project$TriCreator$TransM(
									function (m) {
										return _Utils_update(
											m,
											{
												_: (m._ < 3) ? (m._ + 0.25) : 3,
												aB: (m.aB < 3) ? (m.aB + 0.25) : 3,
												aC: (m.aC < 3) ? (m.aC + 0.25) : 3
											});
									}),
								$author$project$TriCreator$ButtonDown(30),
								$author$project$TriCreator$ButtonDown(36))),
							_Utils_Tuple2(
							'smaller',
							_Utils_Tuple3(
								$author$project$TriCreator$TransM(
									function (m) {
										return _Utils_update(
											m,
											{
												_: (_Utils_cmp(m._, -3) > 0) ? (m._ - 0.25) : (-3),
												aB: (_Utils_cmp(m.aB, -3) > 0) ? (m.aB - 0.25) : (-3),
												aC: (_Utils_cmp(m.aC, -3) > 0) ? (m.aC - 0.25) : (-3)
											});
									}),
								$author$project$TriCreator$ButtonDown(31),
								$author$project$TriCreator$ButtonDown(36)))
						]),
					$elm$core$List$concat(
						A2(
							$elm$core$List$map,
							function (idx) {
								return _List_fromArray(
									[
										_Utils_Tuple2(-30, (-10) * idx),
										_Utils_Tuple2(40, (-10) * idx)
									]);
							},
							A2($elm$core$List$range, 0, 20)))))
			]));
};
var $author$project$TriCreator$yourCode = function (m) {
	return $MacCASOutreach$graphicsvg$GraphicSVG$group(
		_List_fromArray(
			[
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(30, -30),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						A4($MacCASOutreach$graphicsvg$GraphicSVG$rgba, 255, 255, 255, 0.5),
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 260, 85)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-55, 14),
				A3(
					$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
					$MacCASOutreach$graphicsvg$GraphicSVG$solid(1),
					$MacCASOutreach$graphicsvg$GraphicSVG$lightGrey,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$filled,
						$MacCASOutreach$graphicsvg$GraphicSVG$white,
						A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 80, 12)))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-90, 11),
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$filled,
					$author$project$TriCreator$titleColour,
					A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$size,
						10,
						$MacCASOutreach$graphicsvg$GraphicSVG$italic(
							$MacCASOutreach$graphicsvg$GraphicSVG$serif(
								$MacCASOutreach$graphicsvg$GraphicSVG$text('8. Your code!')))))),
				A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$move,
				_Utils_Tuple2(-85, 0),
				$MacCASOutreach$graphicsvg$GraphicSVG$group(
					_Utils_ap(
						_List_fromArray(
							[
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(-10, 0),
								$author$project$ShapeCreateAssets$copiable(
									A2($author$project$TriCreator$stencilString, m, m.bT))),
								A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$move,
								_Utils_Tuple2(0, -10),
								$author$project$ShapeCreateAssets$copiable(
									'  |> ' + (A2($author$project$TriCreator$stampString, m, m.aV) + (((m.aV === 1) ? ($author$project$TriCreator$styleString(m) + ' ') : '') + A2($author$project$TriCreator$clrString, m, m.bj)))))
							]),
						A3(
							$elm$core$List$map2,
							F2(
								function (str, y) {
									return A2(
										$MacCASOutreach$graphicsvg$GraphicSVG$move,
										_Utils_Tuple2(0, y),
										$author$project$ShapeCreateAssets$copiable(str));
								}),
							$elm$core$List$concat(
								A2(
									$elm$core$List$map,
									function (_v0) {
										var flag = _v0.a;
										var t = _v0.b;
										return flag ? _List_fromArray(
											[
												'  ' + A2($author$project$TriCreator$transformString, m, t)
											]) : _List_Nil;
									},
									_List_fromArray(
										[
											_Utils_Tuple2(m.a$, 2),
											_Utils_Tuple2(m.a0, 3),
											_Utils_Tuple2(m.a1, 4),
											_Utils_Tuple2(m.a_, 1),
											_Utils_Tuple2(m.b5, 0),
											_Utils_Tuple2(m.aZ, 5)
										]))),
							_List_fromArray(
								[-20, -30, -40, -50, -60, -70, -80])))))
			]));
};
var $author$project$TriCreator$view = function (model) {
	return _List_fromArray(
		[
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
			0.25,
			A3(
				$MacCASOutreach$graphicsvg$GraphicSVG$graphPaperCustom,
				10,
				1,
				A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 230, 100, 230))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$filled,
			$MacCASOutreach$graphicsvg$GraphicSVG$pink,
			A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 512, 0.5)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$filled,
			$MacCASOutreach$graphicsvg$GraphicSVG$pink,
			A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 512)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(0, 100),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$pink,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 4, 0.5))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(3, 100),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$pink,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(0,100)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(0, -100),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$pink,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 4, 0.5))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(3, -100),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$pink,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(0,-100)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-100, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$pink,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-100, 3),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$pink,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(-100,0)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(100, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$pink,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(100, 3),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$pink,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(100,0)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-200, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$pink,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-200, 3),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$pink,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(-200,0)')))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(200, 0),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$pink,
				A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 0.5, 4))),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(200, 3),
			A2(
				$MacCASOutreach$graphicsvg$GraphicSVG$filled,
				$MacCASOutreach$graphicsvg$GraphicSVG$pink,
				A2(
					$MacCASOutreach$graphicsvg$GraphicSVG$size,
					7,
					$MacCASOutreach$graphicsvg$GraphicSVG$text('(200,0)')))),
			$author$project$TriCreator$shapeFun(model),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-150, 170),
			$author$project$TriCreator$stencils(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-40, 165),
			$author$project$TriCreator$code('|>')),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(80, 169),
			$author$project$TriCreator$stamps(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(230, 169),
			$author$project$TriCreator$colours(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(225, 80),
			$author$project$TriCreator$transforms(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-150, -50),
			$author$project$TriCreator$tweaks(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(-20, -115),
			$author$project$TriCreator$yourCode(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(170, 95),
			$author$project$TriCreator$points(model)),
			A2(
			$MacCASOutreach$graphicsvg$GraphicSVG$move,
			_Utils_Tuple2(133, -70),
			$author$project$TriCreator$angles(model))
		]);
};
var $author$project$Main$view = function (model) {
	return A3(
		$MacCASOutreach$graphicsvg$GraphicSVG$collage,
		512,
		380,
		_Utils_ap(
			function () {
				var _v0 = model.a;
				switch (_v0) {
					case 0:
						return A2(
							$elm$core$List$map,
							$MacCASOutreach$graphicsvg$GraphicSVG$map($author$project$Main$Msg1),
							$author$project$ShapeCreator$view(model.a4));
					case 1:
						return A2(
							$elm$core$List$map,
							$MacCASOutreach$graphicsvg$GraphicSVG$map($author$project$Main$Msg2),
							$author$project$TriCreator$view(model.a5));
					case 2:
						return A2(
							$elm$core$List$map,
							$MacCASOutreach$graphicsvg$GraphicSVG$map($author$project$Main$Msg3),
							$author$project$PolygonCreator$view(model.a6));
					case 3:
						return A2(
							$elm$core$List$map,
							$MacCASOutreach$graphicsvg$GraphicSVG$map($author$project$Main$Msg4),
							$author$project$ArcCreator$view(model.a7));
					case 4:
						return A2(
							$elm$core$List$map,
							$MacCASOutreach$graphicsvg$GraphicSVG$map($author$project$Main$Msg5),
							$author$project$SinCreator$view(model.a8));
					default:
						return A2(
							$elm$core$List$map,
							$MacCASOutreach$graphicsvg$GraphicSVG$map($author$project$Main$Msg6),
							$author$project$TextCreator$view(model.a9));
				}
			}(),
			_Utils_ap(
				_List_fromArray(
					[
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyMouseMoveAt,
						$author$project$Main$MoveInRect,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$move,
							_Utils_Tuple2(0, -190),
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$filled,
								$MacCASOutreach$graphicsvg$GraphicSVG$blank,
								A2($MacCASOutreach$graphicsvg$GraphicSVG$rect, 200, 70))))
					]),
				_List_fromArray(
					[
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyLeave,
						$author$project$Main$Out1,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyEnter,
							$author$project$Main$In1,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$Main$Goto1,
								$MacCASOutreach$graphicsvg$GraphicSVG$group(
									_List_fromArray(
										[
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(-95, -190),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
												0.7,
												A3(
													$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
													$MacCASOutreach$graphicsvg$GraphicSVG$solid(2),
													(!model.a) ? $MacCASOutreach$graphicsvg$GraphicSVG$orange : ((!model.g) ? $MacCASOutreach$graphicsvg$GraphicSVG$blank : $MacCASOutreach$graphicsvg$GraphicSVG$orange),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$filled,
														$author$project$Main$oneColour(model),
														$MacCASOutreach$graphicsvg$GraphicSVG$circle(15))))),
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(-95, -186),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
												0.7,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$filled,
													$author$project$Main$oneAccent(model),
													A2($MacCASOutreach$graphicsvg$GraphicSVG$wedge, 5, 0.75))))
										]))))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyLeave,
						$author$project$Main$Out2,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyEnter,
							$author$project$Main$In2,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$Main$Goto2,
								$MacCASOutreach$graphicsvg$GraphicSVG$group(
									_List_fromArray(
										[
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(-55, -190),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
												0.7,
												A3(
													$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
													$MacCASOutreach$graphicsvg$GraphicSVG$solid(2),
													(model.a === 1) ? $MacCASOutreach$graphicsvg$GraphicSVG$purple : ((!model.i) ? $MacCASOutreach$graphicsvg$GraphicSVG$blank : $MacCASOutreach$graphicsvg$GraphicSVG$purple),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$filled,
														$author$project$Main$twoColour(model),
														$MacCASOutreach$graphicsvg$GraphicSVG$circle(15))))),
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(-58, -188),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
												0.6,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$filled,
													$author$project$Main$twoAccent(model),
													A2($MacCASOutreach$graphicsvg$GraphicSVG$rightTriangle, 10, 10))))
										]))))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyLeave,
						$author$project$Main$Out3,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyEnter,
							$author$project$Main$In3,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$Main$Goto3,
								$MacCASOutreach$graphicsvg$GraphicSVG$group(
									_List_fromArray(
										[
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(-15, -190),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
												0.7,
												A3(
													$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
													$MacCASOutreach$graphicsvg$GraphicSVG$solid(2),
													(model.a === 2) ? $MacCASOutreach$graphicsvg$GraphicSVG$darkBlue : ((!model.h) ? $MacCASOutreach$graphicsvg$GraphicSVG$blank : $MacCASOutreach$graphicsvg$GraphicSVG$darkBlue),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$filled,
														$author$project$Main$threeColour(model),
														$MacCASOutreach$graphicsvg$GraphicSVG$circle(15))))),
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(-17, -185),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
												0.6,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$filled,
													$author$project$Main$threeAccent(model),
													$MacCASOutreach$graphicsvg$GraphicSVG$polygon(
														_List_fromArray(
															[
																_Utils_Tuple2(0, 5),
																_Utils_Tuple2(7, 5),
																_Utils_Tuple2(8, 2),
																_Utils_Tuple2(5, 0),
																_Utils_Tuple2(7, -3),
																_Utils_Tuple2(2, -3)
															])))))
										]))))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyLeave,
						$author$project$Main$Out4,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyEnter,
							$author$project$Main$In4,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$Main$Goto4,
								$MacCASOutreach$graphicsvg$GraphicSVG$group(
									_List_fromArray(
										[
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(25, -190),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
												0.7,
												A3(
													$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
													$MacCASOutreach$graphicsvg$GraphicSVG$solid(2),
													(model.a === 3) ? $MacCASOutreach$graphicsvg$GraphicSVG$darkGreen : ((!model.f) ? $MacCASOutreach$graphicsvg$GraphicSVG$blank : $MacCASOutreach$graphicsvg$GraphicSVG$darkGreen),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$filled,
														$author$project$Main$fourColour(model),
														$MacCASOutreach$graphicsvg$GraphicSVG$circle(15))))),
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(25, -185),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
												0.5,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$scale,
													0.3,
													A3(
														$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
														$MacCASOutreach$graphicsvg$GraphicSVG$solid(2),
														$author$project$Main$fourAccent(model),
														A2(
															$MacCASOutreach$graphicsvg$GraphicSVG$curve,
															_Utils_Tuple2(0, 0),
															_List_fromArray(
																[
																	A2(
																	$MacCASOutreach$graphicsvg$GraphicSVG$Pull,
																	_Utils_Tuple2(25, 15),
																	_Utils_Tuple2(25, 0)),
																	A2(
																	$MacCASOutreach$graphicsvg$GraphicSVG$Pull,
																	_Utils_Tuple2(25, -15),
																	_Utils_Tuple2(0, 0)),
																	A2(
																	$MacCASOutreach$graphicsvg$GraphicSVG$Pull,
																	_Utils_Tuple2(-25, 15),
																	_Utils_Tuple2(-25, 0)),
																	A2(
																	$MacCASOutreach$graphicsvg$GraphicSVG$Pull,
																	_Utils_Tuple2(-25, -15),
																	_Utils_Tuple2(0, 0))
																]))))))
										]))))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyLeave,
						$author$project$Main$Out5,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyEnter,
							$author$project$Main$In5,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$Main$Goto5,
								$MacCASOutreach$graphicsvg$GraphicSVG$group(
									_List_fromArray(
										[
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(65, -190),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
												0.7,
												A3(
													$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
													$MacCASOutreach$graphicsvg$GraphicSVG$solid(2),
													(model.a === 4) ? $MacCASOutreach$graphicsvg$GraphicSVG$darkRed : ((!model.e) ? $MacCASOutreach$graphicsvg$GraphicSVG$blank : $MacCASOutreach$graphicsvg$GraphicSVG$darkRed),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$filled,
														$author$project$Main$fiveColour(model),
														$MacCASOutreach$graphicsvg$GraphicSVG$circle(15))))),
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(65, -185),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
												0.5,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$scale,
													0.3,
													A3(
														$MacCASOutreach$graphicsvg$GraphicSVG$outlined,
														$MacCASOutreach$graphicsvg$GraphicSVG$solid(3),
														$author$project$Main$fiveAccent(model),
														A2(
															$MacCASOutreach$graphicsvg$GraphicSVG$curve,
															_Utils_Tuple2(30, 0),
															_List_fromArray(
																[
																	A2(
																	$MacCASOutreach$graphicsvg$GraphicSVG$Pull,
																	_Utils_Tuple2(14, 40),
																	_Utils_Tuple2(0, 0)),
																	A2(
																	$MacCASOutreach$graphicsvg$GraphicSVG$Pull,
																	_Utils_Tuple2(-10, -30),
																	_Utils_Tuple2(-30, 0))
																]))))))
										]))))),
						A2(
						$MacCASOutreach$graphicsvg$GraphicSVG$notifyLeave,
						$author$project$Main$Out6,
						A2(
							$MacCASOutreach$graphicsvg$GraphicSVG$notifyEnter,
							$author$project$Main$In6,
							A2(
								$MacCASOutreach$graphicsvg$GraphicSVG$notifyTap,
								$author$project$Main$Goto6,
								$MacCASOutreach$graphicsvg$GraphicSVG$group(
									_List_fromArray(
										[
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(105, -190),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
												0.7,
												A3(
													$MacCASOutreach$graphicsvg$GraphicSVG$addOutline,
													$MacCASOutreach$graphicsvg$GraphicSVG$solid(2),
													(model.a === 5) ? A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 0, 170, 130) : ((!model.d) ? $MacCASOutreach$graphicsvg$GraphicSVG$blank : A3($MacCASOutreach$graphicsvg$GraphicSVG$rgb, 0, 130, 130)),
													A2(
														$MacCASOutreach$graphicsvg$GraphicSVG$filled,
														$author$project$Main$sixColour(model),
														$MacCASOutreach$graphicsvg$GraphicSVG$circle(15))))),
											A2(
											$MacCASOutreach$graphicsvg$GraphicSVG$move,
											_Utils_Tuple2(98, -187),
											A2(
												$MacCASOutreach$graphicsvg$GraphicSVG$makeTransparent,
												0.5,
												A2(
													$MacCASOutreach$graphicsvg$GraphicSVG$filled,
													$author$project$Main$sixAccent(model),
													$MacCASOutreach$graphicsvg$GraphicSVG$text('Aa'))))
										])))))
					]))));
};
var $author$project$Main$main = A2(
	$MacCASOutreach$graphicsvg$GraphicSVG$EllieApp$gameApp,
	$author$project$Main$Tick,
	{cY: $author$project$Main$init, db: 'Shape Creator', dh: $author$project$Main$update, dk: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(0))(0)}});}(this));