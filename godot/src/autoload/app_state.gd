class_name ImmutableTowersAppState
extends Node

const AccountRepository = preload("res://src/persistence/account_repository.gd")
const DomainCatalog = preload("res://src/domain/domain_catalog.gd")
const DomainIds = preload("res://src/domain/domain_ids.gd")
const MetaShop = preload("res://src/economy/meta_shop.gd")
const RunScore = preload("res://src/gameplay/run_score.gd")
const RunRewards = preload("res://src/gameplay/run_rewards.gd")

var repository: Variant
var catalog: Variant
var active_account: Dictionary = {}
var selected_map_id := "planicie_serena"
var resume_pending_run := false
var last_run_gem_reward := 0
var last_run_wave_reached := 0

func _ready() -> void:
	repository = AccountRepository.new()
	catalog = DomainCatalog.new()
	catalog.load_default()
	_load_or_create_account()

func create_account(account_id: String, name: String) -> String:
	var error: String = repository.create_account(account_id, name)
	if error.is_empty():
		select_account(account_id)
	return error

func create_account_with_generated_id(name: String) -> String:
	return create_account(repository.generate_account_id("account"), name)

func select_account(account_id: String) -> String:
	var account: Variant = repository.load_account(account_id)
	if not account is Dictionary:
		return "Conta inexistente"
	var error: String = repository.select_account(account_id)
	if error.is_empty():
		active_account = account
		selected_map_id = map_for_next_run()
	return error

func save_active_account() -> String:
	return repository.save_account(active_account) if not active_account.is_empty() else "Sem conta ativa"

func logout() -> String:
	var save_error := "" if active_account.is_empty() else save_active_account()
	if not save_error.is_empty():
		return save_error
	var error: String = repository.logout_account()
	if error.is_empty():
		active_account = {}
		resume_pending_run = false
	return error

func remember_account() -> bool:
	return bool(repository.load_index().get("remember_account", true))

func set_remember_account(value: bool) -> String:
	return repository.set_remember_account(value, str(active_account.get("account_id", "")))

func settings() -> Dictionary:
	var values := {
		"fullscreen": false,
		"reduced_effects": false,
		"damage_numbers": true
	}
	var stored: Variant = active_account.get("settings")
	if stored is Dictionary:
		for key: String in values:
			values[key] = bool(stored.get(key, values[key]))
	return values

func set_setting(setting_id: String, value: bool) -> String:
	if setting_id not in ["fullscreen", "reduced_effects", "damage_numbers"]:
		return "Opcao desconhecida"
	var values := settings()
	values[setting_id] = value
	active_account["settings"] = values
	return save_active_account()

func has_pending_run() -> bool:
	var pending: Variant = active_account.get("pending_run")
	return pending is Dictionary and pending.get("schema", "") == "immutable-towers-run"

func pending_run() -> Variant:
	return active_account.get("pending_run") if has_pending_run() else null

func save_pending_run(snapshot: Dictionary) -> String:
	active_account["pending_run"] = snapshot.duplicate(true)
	return save_active_account()

func clear_pending_run() -> String:
	active_account["pending_run"] = null
	resume_pending_run = false
	return save_active_account()

func rename_active_account(name: String) -> String:
	var error: String = repository.rename_account(str(active_account.get("account_id", "")), name)
	if error.is_empty():
		active_account = repository.load_account(str(active_account.get("account_id")))
	return error

func set_selected_mode(mode_id: String) -> String:
	if not mode_unlocked(mode_id):
		return "Modo bloqueado ate ao nivel " + str(catalog.mode(mode_id).get("minimum_level", 1))
	active_account["selected_mode"] = mode_id
	selected_map_id = map_for_next_run()
	return save_active_account()

func mode_unlocked(mode_id: String) -> bool:
	if not catalog.modes.has(mode_id):
		return false
	return account_level() >= int(catalog.mode(mode_id).get("minimum_level", 1))

func account_level() -> int:
	return int(active_account.get("meta_progress", {}).get("level", 1))

func unlocked_towers() -> PackedStringArray:
	return PackedStringArray(active_account.get("meta_progress", {}).get("unlocked_tower_ids", ["sentinela"]))

func towers_for_selected_mode() -> PackedStringArray:
	return DomainIds.TOWERS.duplicate() if active_account.get("selected_mode", "history") == "sandbox" else unlocked_towers()

func map_for_next_run() -> String:
	var progress: Dictionary = active_account.get("meta_progress", {})
	var index := int(progress.get("history_stage", 1)) - 1 if active_account.get("selected_mode", "history") == "history" else int(progress.get("map_rotation", 0))
	return DomainIds.MAPS[posmod(index, DomainIds.MAPS.size())]

func has_custom_map() -> bool:
	var document: Variant = active_account.get("custom_map")
	return document is Dictionary and document.get("schema", "") == "immutable-towers-custom-map"

func custom_map_for_run() -> Dictionary:
	return active_account.get("custom_map", {}) if bool(active_account.get("use_custom_map", false)) and has_custom_map() else {}

func run_context_for_selected_mode() -> Dictionary:
	var progress: Dictionary = active_account.get("meta_progress", {})
	return {
		"campaign_profile": active_account.get("selected_mode", "history") == "history",
		"chapter": maxi(1, int(progress.get("history_chapter", 1))),
		"stage": clampi(int(progress.get("history_stage", 1)), 1, 5)
	}

func select_custom_map(document: Dictionary) -> String:
	active_account["custom_map"] = document.duplicate(true)
	active_account["use_custom_map"] = true
	return save_active_account()

func select_official_maps() -> String:
	active_account["use_custom_map"] = false
	return save_active_account()

func buy_chest(chest_id: String) -> Dictionary:
	if active_account.get("pending_reward") is Dictionary:
		return {"error": "Recolhe primeiro a recompensa pendente"}
	var result := MetaShop.buy_chest(chest_id, active_account.get("meta_progress", {}))
	if str(result.get("error", "")).is_empty():
		active_account["meta_progress"] = result.get("progress")
		active_account["shop_state"] = result.get("progress").duplicate(true)
		active_account["pending_reward"] = result.get("reward")
		result["save_error"] = save_active_account()
	return result

func claim_pending_reward() -> String:
	if not active_account.get("pending_reward") is Dictionary:
		return "Nao existe recompensa pendente"
	active_account["pending_reward"] = null
	return save_active_account()

func delete_active_account() -> String:
	var account_id := str(active_account.get("account_id", ""))
	var error: String = repository.delete_account(account_id)
	if error.is_empty():
		active_account = {}
		_load_or_create_account()
	return error

func fuse_tempestade() -> Dictionary:
	var result := MetaShop.fuse_tempestade(active_account.get("meta_progress", {}))
	if str(result.get("error", "")).is_empty():
		active_account["meta_progress"] = result.get("progress")
		active_account["shop_state"] = result.get("progress").duplicate(true)
		result["save_error"] = save_active_account()
	return result

func complete_run(simulation: Variant) -> String:
	var profile: Dictionary = active_account.get("profile", {})
	var progress: Dictionary = active_account.get("meta_progress", {}).duplicate(true)
	var won: bool = simulation.victory
	var mode_id := str(simulation.mode_id)
	var survived_waves := RunRewards.completed_waves(simulation)
	last_run_wave_reached = RunRewards.wave_reached(simulation)
	last_run_gem_reward = 0
	var score := _score(simulation, survived_waves)
	profile["games"] = int(profile.get("games", 0)) + 1
	profile["wins"] = int(profile.get("wins", 0)) + (1 if won else 0)
	profile["losses"] = int(profile.get("losses", 0)) + (0 if won else 1)
	profile["best_score"] = maxi(int(profile.get("best_score", 0)), score)
	if mode_id == "infinite":
		last_run_gem_reward = RunRewards.infinite_gems(last_run_wave_reached)
		progress["gems"] = int(progress.get("gems", 0)) + last_run_gem_reward
		progress["best_infinite_wave"] = maxi(int(progress.get("best_infinite_wave", 0)), last_run_wave_reached)
		progress["map_rotation"] = int(progress.get("map_rotation", 0)) + 1
	elif won:
		progress = _apply_victory_progress(progress, mode_id)
	elif mode_id != "history":
		progress["map_rotation"] = int(progress.get("map_rotation", 0)) + 1
	var leaderboard: Array = active_account.get("leaderboard", [])
	leaderboard.append({
		"name": profile.get("name", active_account.get("name", "Jogador")),
		"mode_id": mode_id,
		"score": score,
		"waves": last_run_wave_reached if mode_id == "infinite" else survived_waves
	})
	leaderboard.sort_custom(func(left: Dictionary, right: Dictionary) -> bool: return int(left.get("score", 0)) > int(right.get("score", 0)))
	active_account["profile"] = profile
	active_account["meta_progress"] = progress
	active_account["shop_state"] = progress.duplicate(true)
	active_account["leaderboard"] = leaderboard.slice(0, mini(10, leaderboard.size()))
	active_account["pending_run"] = null
	selected_map_id = map_for_next_run()
	return save_active_account()

func _apply_victory_progress(progress: Dictionary, mode_id: String) -> Dictionary:
	var updated := progress.duplicate(true)
	var reward := 0
	if mode_id == "history":
		var chapter := int(progress.get("history_chapter", 1))
		var stage := int(progress.get("history_stage", 1))
		reward = 24 + chapter * 10 + stage * 6
		updated["completed_stages"] = int(progress.get("completed_stages", 0)) + 1
		updated["level"] = maxi(int(progress.get("level", 1)), 1 + floori(float(updated["completed_stages"]) / 2.0))
		if stage < 5:
			updated["history_stage"] = stage + 1
		else:
			updated["history_chapter"] = chapter + 1
			updated["history_stage"] = 1
	else:
		match mode_id:
			"challenge": reward = 42
			"boss": reward = 60
			"sandbox": reward = 8
		updated["map_rotation"] = int(progress.get("map_rotation", 0)) + 1
		var wins := int(active_account.get("profile", {}).get("wins", 0))
		updated["level"] = maxi(int(progress.get("level", 1)), 1 + floori(float(int(progress.get("completed_stages", 0)) + wins) / 2.0))
	updated["gems"] = int(progress.get("gems", 0)) + reward
	return updated

func _score(simulation: Variant, survived_waves: int) -> int:
	return RunScore.calculate(simulation, survived_waves)

func _load_or_create_account() -> void:
	var index: Dictionary = repository.load_index()
	var last_account: Variant = index.get("last_account_id") if bool(index.get("remember_account", true)) else null
	var account_id := "" if last_account == null else str(last_account)
	var loaded: Variant = repository.load_account(account_id)
	if loaded is Dictionary:
		active_account = loaded
		selected_map_id = map_for_next_run()
