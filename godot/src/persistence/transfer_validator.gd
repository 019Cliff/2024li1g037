class_name TransferValidator
extends RefCounted

const SCHEMA_ID := "immutable-towers-transfer"
const SUPPORTED_VERSION := 1

static func validate_document(document: Variant) -> PackedStringArray:
	var errors := PackedStringArray()
	if not document is Dictionary:
		errors.append("transfer: root must be an object")
		return errors

	var root: Dictionary = document
	if root.get("schema", "") != SCHEMA_ID:
		errors.append("transfer: unsupported schema")

	var version: Variant = root.get("version")
	if not _is_integer_number(version) or int(version) != SUPPORTED_VERSION:
		errors.append("transfer: unsupported version")

	_validate_source(root.get("source"), errors)
	_validate_optional_array(root, "accounts", errors)
	_validate_optional_array(root, "warnings", errors)
	return errors

static func _validate_source(value: Variant, errors: PackedStringArray) -> void:
	if not value is Dictionary:
		errors.append("transfer: source must be an object")
		return
	var source: Dictionary = value
	if source.get("game", "") != "haskell-gloss":
		errors.append("transfer: invalid source game")
	var exporter_version: Variant = source.get("exporter_version")
	if not _is_integer_number(exporter_version) or int(exporter_version) < 1:
		errors.append("transfer: invalid exporter version")

static func _validate_optional_array(root: Dictionary, field: String, errors: PackedStringArray) -> void:
	if root.has(field) and not root[field] is Array:
		errors.append("transfer: " + field + " must be an array")

static func _is_integer_number(value: Variant) -> bool:
	if value is int:
		return true
	if value is float:
		return is_finite(value) and value == floor(value)
	return false
