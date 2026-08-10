class_name AccountRepository
extends RefCounted

const AtomicJsonStore = preload("res://src/persistence/atomic_json_store.gd")
const AccountImportService = preload("res://src/persistence/account_import_service.gd")
const TransferImportPlanner = preload("res://src/persistence/transfer_import_planner.gd")
const LegacyRunConverter = preload("res://src/persistence/legacy_run_converter.gd")

var root: String

func _init(storage_root: String = "user://accounts") -> void:
	root = storage_root.trim_suffix("/")

func default_index() -> Dictionary:
	return {
		"schema": "immutable-towers-account-index",
		"version": 1,
		"accounts": [],
		"last_account_id": null,
		"remember_account": true,
		"imported_fingerprints": []
	}

func new_account(account_id: String, name: String) -> Dictionary:
	return {
		"schema": "immutable-towers-account",
		"version": 1,
		"account_id": account_id,
		"name": name.strip_edges(),
		"profile": {"name": name.strip_edges(), "games": 0, "wins": 0, "losses": 0, "best_score": 0},
		"meta_progress": _default_progress(),
		"leaderboard": [],
		"selected_mode": "history",
		"shop_state": _default_progress(),
		"pending_run": null,
		"pending_reward": null,
		"settings": _default_settings()
	}

func create_account(account_id: String, name: String) -> String:
	var validation := validate_name(name)
	if not validation.is_empty():
		return validation
	if not _valid_id(account_id):
		return "Identificador de conta invalido"
	var index := load_index()
	for summary: Variant in index.get("accounts", []):
		if str(summary.get("account_id", "")) == account_id:
			return "Essa conta ja existe"
		if _normalize_name(str(summary.get("name", ""))) == _normalize_name(name):
			return "Esse nome ja esta a ser usado"
	var account := new_account(account_id, name)
	var error := save_account(account)
	if not error.is_empty():
		return error
	var summaries: Array = index.get("accounts", [])
	summaries.append(_summary(account))
	index["accounts"] = summaries
	index["last_account_id"] = account_id if bool(index.get("remember_account", true)) else null
	return AtomicJsonStore.save(index, _index_path())

func generate_account_id(prefix: String = "account") -> String:
	var clean_prefix := prefix.to_lower()
	if not _valid_id(clean_prefix):
		clean_prefix = "account"
	var crypto := Crypto.new()
	for _attempt in 8:
		var random_bytes := crypto.generate_random_bytes(8)
		if random_bytes.is_empty():
			continue
		var candidate := clean_prefix + "-" + random_bytes.hex_encode()
		if load_account(candidate) == null:
			return candidate
	return clean_prefix + "-" + str(Time.get_ticks_usec())

func save_account(account: Dictionary) -> String:
	var errors := validate_account(account)
	if not errors.is_empty():
		return errors[0]
	var error := AtomicJsonStore.save(account, _account_path(str(account.get("account_id"))))
	if not error.is_empty():
		return error
	var index := load_index()
	var summaries: Array = index.get("accounts", [])
	var replacement := _summary(account)
	var found := false
	for position in summaries.size():
		if str(summaries[position].get("account_id", "")) == account.get("account_id"):
			summaries[position] = replacement
			found = true
	if not found:
		summaries.append(replacement)
	index["accounts"] = summaries
	return AtomicJsonStore.save(index, _index_path())

func load_account(account_id: String) -> Variant:
	var account: Variant = AtomicJsonStore.load_with_backup(_account_path(account_id))
	return account if account is Dictionary and validate_account(account).is_empty() else null

func load_index() -> Dictionary:
	var index: Variant = AtomicJsonStore.load_with_backup(_index_path())
	if index is Dictionary and index.get("schema") == "immutable-towers-account-index" and int(index.get("version", 0)) == 1:
		return index
	return default_index()

func select_account(account_id: String) -> String:
	if load_account(account_id) == null:
		return "Conta inexistente"
	var index := load_index()
	index["last_account_id"] = account_id if bool(index.get("remember_account", true)) else null
	return AtomicJsonStore.save(index, _index_path())

func set_remember_account(value: bool, active_account_id: String = "") -> String:
	var index := load_index()
	index["remember_account"] = value
	index["last_account_id"] = active_account_id if value and not active_account_id.is_empty() else null
	return AtomicJsonStore.save(index, _index_path())

func logout_account() -> String:
	var index := load_index()
	index["last_account_id"] = null
	return AtomicJsonStore.save(index, _index_path())

func rename_account(account_id: String, name: String) -> String:
	var validation := validate_name(name)
	if not validation.is_empty():
		return validation
	var index := load_index()
	for summary: Variant in index.get("accounts", []):
		if str(summary.get("account_id", "")) != account_id and _normalize_name(str(summary.get("name", ""))) == _normalize_name(name):
			return "Esse nome ja esta a ser usado"
	var account: Variant = load_account(account_id)
	if not account is Dictionary:
		return "Conta inexistente"
	account["name"] = name.strip_edges()
	var profile: Dictionary = account.get("profile", {})
	profile["name"] = name.strip_edges()
	account["profile"] = profile
	return save_account(account)

func export_account(account_id: String, output_path: String) -> String:
	var account: Variant = load_account(account_id)
	if not account is Dictionary:
		return "Conta inexistente"
	return AtomicJsonStore.save({
		"schema": "immutable-towers-account-export",
		"version": 1,
		"exported_account": account
	}, output_path)

func import_document(document: Dictionary, collision_policy: String = "cancel") -> Dictionary:
	var classification := AccountImportService.classify(document)
	if classification.get("status") != "ready":
		return classification
	if classification.get("kind") == "haskell_transfer":
		var transfer_result := import_transfer(document, collision_policy == "replace")
		transfer_result["import_kind"] = "haskell_transfer"
		return transfer_result
	var native_result := import_account_export(document, collision_policy)
	native_result["import_kind"] = "native_account"
	return native_result

func import_account_export(document: Dictionary, collision_policy: String = "cancel") -> Dictionary:
	var classification := AccountImportService.classify(document)
	if classification.get("status") != "ready" or classification.get("kind") != "native_account":
		return classification
	var account := _normalize_imported_account(classification.get("account", {}))
	var validation := validate_account(account)
	if not validation.is_empty():
		return {"status": "invalid", "errors": validation}
	var original_id := str(account.get("account_id", ""))
	var collision := load_account(original_id) is Dictionary
	if collision and collision_policy == "cancel":
		return {
			"status": "confirmation_required",
			"conflict": "account_id",
			"account_id": original_id,
			"account_name": account.get("name", "Jogador"),
			"options": ["import_as_new", "replace", "cancel"]
		}
	if collision_policy not in ["cancel", "import_as_new", "replace"]:
		return {"status": "invalid", "errors": ["Politica de colisao desconhecida"]}
	if collision and collision_policy == "import_as_new":
		account["account_id"] = generate_account_id("account")
		var imported_name := _unique_import_name(str(account.get("name", "Jogador")))
		account["name"] = imported_name
		var profile: Dictionary = account.get("profile", {})
		profile["name"] = imported_name
		account["profile"] = profile
	var error := save_account(account)
	if not error.is_empty():
		return {"status": "error", "error": error}
	return {
		"status": "imported",
		"imported": 1,
		"account_id": account.get("account_id"),
		"source_account_id": original_id,
		"collision_policy": collision_policy if collision else "none"
	}

func delete_account(account_id: String) -> String:
	if not _valid_id(account_id):
		return "Identificador de conta invalido"
	var account_directory := root + "/" + account_id
	for suffix in ["", ".tmp", ".bak"]:
		var path: String = account_directory + "/profile-v1.json" + str(suffix)
		if FileAccess.file_exists(path) and DirAccess.remove_absolute(path) != OK:
			return "Nao foi possivel eliminar os dados da conta"
	var absolute_directory := ProjectSettings.globalize_path(account_directory)
	if DirAccess.dir_exists_absolute(absolute_directory):
		DirAccess.remove_absolute(absolute_directory)
	var index := load_index()
	var summaries: Array = []
	for summary: Variant in index.get("accounts", []):
		if str(summary.get("account_id", "")) != account_id:
			summaries.append(summary)
	index["accounts"] = summaries
	if index.get("last_account_id") == account_id:
		index["last_account_id"] = null if summaries.is_empty() else summaries[0].get("account_id")
	return AtomicJsonStore.save(index, _index_path())

func import_transfer(document: Dictionary, confirm_overwrite: bool = false) -> Dictionary:
	var index := load_index()
	var fingerprints := PackedStringArray(index.get("imported_fingerprints", []))
	var source_accounts := _source_accounts(document)
	var transfer_newest := _maximum_completed_stages(source_accounts)
	var local_newest := 0
	for summary: Variant in index.get("accounts", []):
		var local: Variant = load_account(str(summary.get("account_id", "")))
		if local is Dictionary:
			local_newest = maxi(local_newest, int(local.get("meta_progress", {}).get("completed_stages", 0)))
	var plan := TransferImportPlanner.plan_import(document, fingerprints, local_newest > transfer_newest, confirm_overwrite)
	if plan.get("status") != "ready":
		return plan
	var imported := 0
	for source: Variant in source_accounts:
		if not source is Dictionary:
			continue
		var account := _normalize_imported_account(source)
		var save_error := save_account(account)
		if not save_error.is_empty():
			return {"status": "error", "error": save_error, "imported": imported}
		imported += 1
	index = load_index()
	var receipts: Array = index.get("imported_fingerprints", [])
	receipts.append(str(plan.get("fingerprint")))
	index["imported_fingerprints"] = receipts
	var index_error := AtomicJsonStore.save(index, _index_path())
	return {"status": "imported", "imported": imported, "error": index_error, "fingerprint": plan.get("fingerprint")}

func _source_accounts(document: Dictionary) -> Array:
	var accounts: Array = document.get("accounts", []).duplicate(true)
	if not accounts.is_empty():
		return accounts
	var legacy: Variant = document.get("legacy_global")
	if not legacy is Dictionary or not legacy.get("meta") is Dictionary:
		return accounts
	var meta: Dictionary = legacy.get("meta")
	var profile: Dictionary = meta.get("profile", {})
	accounts.append({
		"account_id": "legacy-global",
		"name": profile.get("name", "Jogador Haskell"),
		"profile": profile,
		"meta_progress": meta.get("meta_progress", _default_progress()),
		"leaderboard": meta.get("leaderboard", []),
		"selected_mode": meta.get("selected_mode", "history"),
		"pending_run": legacy.get("pending_run")
	})
	return accounts

func validate_name(name: String) -> String:
	var clean := " ".join(name.strip_edges().split(" ", false))
	if clean.is_empty():
		return "O nome da conta nao pode ficar vazio"
	if clean.length() > 16:
		return "O nome da conta pode ter no maximo 16 caracteres"
	return ""

func validate_account(account: Variant) -> PackedStringArray:
	var errors := PackedStringArray()
	if not account is Dictionary:
		errors.append("Conta invalida")
		return errors
	if account.get("schema", "") != "immutable-towers-account" or int(account.get("version", 0)) != 1:
		errors.append("Versao de conta nao suportada")
	if not _valid_id(str(account.get("account_id", ""))):
		errors.append("Identificador de conta invalido")
	var name_error := validate_name(str(account.get("name", "")))
	if not name_error.is_empty():
		errors.append(name_error)
	if not account.get("profile") is Dictionary:
		errors.append("Perfil invalido")
	if not account.get("meta_progress") is Dictionary or not account.get("leaderboard") is Array:
		errors.append("Dados de progresso invalidos")
	if not account.get("shop_state") is Dictionary or not account.get("settings") is Dictionary:
		errors.append("Dados locais invalidos")
	var pending_run: Variant = account.get("pending_run")
	if pending_run != null and not pending_run is Dictionary:
		errors.append("Partida pendente invalida")
	var pending_reward: Variant = account.get("pending_reward")
	if pending_reward != null and not pending_reward is Dictionary:
		errors.append("Recompensa pendente invalida")
	return errors

func _normalize_imported_account(source: Dictionary) -> Dictionary:
	var account := source.duplicate(true)
	account["schema"] = "immutable-towers-account"
	account["version"] = 1
	var defaults := new_account(str(account.get("account_id", "")), str(account.get("name", "Jogador")))
	var profile: Dictionary = defaults.get("profile", {})
	if account.get("profile") is Dictionary:
		profile.merge(account.get("profile"), true)
	profile["name"] = str(account.get("name", profile.get("name", "Jogador")))
	account["profile"] = profile
	var progress: Dictionary = _default_progress()
	if account.get("meta_progress") is Dictionary:
		progress.merge(account.get("meta_progress"), true)
	account["meta_progress"] = progress
	if not account.get("leaderboard") is Array:
		account["leaderboard"] = []
	if not account.has("selected_mode"):
		account["selected_mode"] = "history"
	if not account.get("shop_state") is Dictionary:
		account["shop_state"] = progress.duplicate(true)
	if not account.has("pending_reward"):
		account["pending_reward"] = null
	var settings := _default_settings()
	if account.get("settings") is Dictionary:
		settings.merge(account.get("settings"), true)
	account["settings"] = settings
	var pending: Variant = account.get("pending_run")
	if pending is Dictionary and pending.get("schema", "") != "immutable-towers-run":
		account["pending_run"] = LegacyRunConverter.convert(
			pending,
			str(account.get("selected_mode", "history")),
			PackedStringArray(account.get("meta_progress", {}).get("unlocked_tower_ids", ["sentinela"]))
		)
	return account

func _unique_import_name(source_name: String) -> String:
	var names: Dictionary[String, bool] = {}
	for summary: Variant in load_index().get("accounts", []):
		names[_normalize_name(str(summary.get("name", "")))] = true
	var base := source_name.strip_edges()
	if base.is_empty():
		base = "Jogador"
	for number in range(1, 1000):
		var suffix := " copia" if number == 1 else " copia " + str(number)
		var candidate := base.left(maxi(1, 16 - suffix.length())) + suffix
		if not names.has(_normalize_name(candidate)):
			return candidate
	return "Conta importada"

func _maximum_completed_stages(accounts: Array) -> int:
	var maximum := 0
	for account: Variant in accounts:
		if account is Dictionary:
			maximum = maxi(maximum, int(account.get("meta_progress", {}).get("completed_stages", 0)))
	return maximum

func _summary(account: Dictionary) -> Dictionary:
	return {
		"account_id": account.get("account_id"),
		"name": account.get("name"),
		"level": int(account.get("meta_progress", {}).get("level", 1)),
		"last_map_id": null
	}

func _default_progress() -> Dictionary:
	return {
		"gems": 40,
		"level": 1,
		"unlocked_tower_ids": ["sentinela"],
		"fused_tower_ids": [],
		"history_chapter": 1,
		"history_stage": 1,
		"completed_stages": 0,
		"best_infinite_wave": 0,
		"map_rotation": 0
	}

func _default_settings() -> Dictionary:
	return {
		"fullscreen": false,
		"reduced_effects": false,
		"damage_numbers": true
	}

func _normalize_name(value: String) -> String:
	return " ".join(value.to_lower().strip_edges().split(" ", false))

func _valid_id(value: String) -> bool:
	if value.is_empty():
		return false
	for character in value:
		if not (character >= "a" and character <= "z") and not (character >= "0" and character <= "9") and character not in ["_", "-"]:
			return false
	return true

func _index_path() -> String:
	return root + "/accounts-index-v1.json"

func _account_path(account_id: String) -> String:
	return root + "/" + account_id + "/profile-v1.json"
