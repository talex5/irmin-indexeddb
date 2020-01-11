//Provides: caml_encore_is_a_sub
function caml_encore_is_a_sub(va, valen, vb, vblen) {
	var a = va.data.buffer;
	var b = vb.data.buffer;
	var astart = va.data.byteOffset;
	var bstart = vb.data.byteOffset;
	return a === b && astart >= bstart && astart + valen <= bstart + vblen
}

//Provides: caml_encore_bigarray_equal
function caml_encore_bigarray_equal(va, vb) {
	var a = va.data.buffer;
	var b = vb.data.buffer;
	return a === b
}
