class_name TransferImportPlanner
extends RefCounted

const TransferValidator = preload("res://src/persistence/transfer_validator.gd")

static func fingerprint(document: Dictionary) -> String:
	var context := HashingContext.new()
	context.start(HashingContext.HASH_SHA256)
	context.update(JSON.stringify(document).to_utf8_buffer())
	return context.finish().hex_encode()

static func plan_import(
	document: Variant,
	imported_fingerprints: PackedStringArray,
	godot_progress_is_newer: bool,
	confirm_overwrite: bool
) -> Dictionary:
	var errors: PackedStringArray = TransferValidator.validate_document(document)
	if not errors.is_empty():
		return {"status": "invalid", "errors": errors}

	var transfer: Dictionary = document
	var transfer_fingerprint := fingerprint(transfer)
	if transfer_fingerprint in imported_fingerprints:
		return {"status": "already_imported", "fingerprint": transfer_fingerprint}
	if godot_progress_is_newer and not confirm_overwrite:
		return {"status": "confirmation_required", "fingerprint": transfer_fingerprint}
	return {"status": "ready", "fingerprint": transfer_fingerprint}
