class_name AtomicJsonStore
extends RefCounted

static func save(document: Dictionary, path: String) -> String:
	var temporary := path + ".tmp"
	var backup := path + ".bak"
	var directory := path.get_base_dir()
	if not directory.is_empty():
		var directory_error := DirAccess.make_dir_recursive_absolute(ProjectSettings.globalize_path(directory))
		if directory_error != OK and directory_error != ERR_ALREADY_EXISTS:
			return "Nao foi possivel criar a pasta de dados"
	var file := FileAccess.open(temporary, FileAccess.WRITE)
	if file == null:
		return "Nao foi possivel criar o ficheiro temporario"
	file.store_string(JSON.stringify(document))
	file.close()
	if not read(path + ".tmp") is Dictionary:
		return "Validacao do ficheiro temporario falhou"
	if FileAccess.file_exists(path):
		if DirAccess.copy_absolute(path, backup) != OK:
			return "Nao foi possivel criar backup"
		if DirAccess.remove_absolute(path) != OK:
			return "Nao foi possivel substituir o ficheiro anterior"
	return "" if DirAccess.rename_absolute(temporary, path) == OK else "Nao foi possivel promover o ficheiro"

static func load_with_backup(path: String) -> Variant:
	var primary: Variant = read(path)
	return primary if primary is Dictionary else read(path + ".bak")

static func read(path: String) -> Variant:
	var file := FileAccess.open(path, FileAccess.READ)
	if file == null:
		return null
	var parser := JSON.new()
	if parser.parse(file.get_as_text()) != OK:
		return null
	return parser.data
