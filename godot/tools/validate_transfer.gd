extends SceneTree

const TransferValidator = preload("res://src/persistence/transfer_validator.gd")

func _init() -> void:
	var arguments := OS.get_cmdline_user_args()
	if arguments.size() != 1:
		push_error("Usage: --script res://tools/validate_transfer.gd -- <transfer.json>")
		quit(2)
		return
	var file := FileAccess.open(arguments[0], FileAccess.READ)
	if file == null:
		push_error("Transfer file could not be opened: " + arguments[0])
		quit(2)
		return
	var document: Variant = JSON.parse_string(file.get_as_text())
	var errors: PackedStringArray = TransferValidator.validate_document(document)
	if not errors.is_empty():
		for error in errors:
			push_error(error)
		quit(1)
		return
	print("Transfer validation: PASS")
	quit(0)
