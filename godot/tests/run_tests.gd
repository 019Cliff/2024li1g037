extends SceneTree

const DomainIds = preload("res://src/domain/domain_ids.gd")
const DomainCatalog = preload("res://src/domain/domain_catalog.gd")
const TransferValidator = preload("res://src/persistence/transfer_validator.gd")
const TransferImportPlanner = preload("res://src/persistence/transfer_import_planner.gd")
const VerticalSliceSimulation = preload("res://src/domain/vertical_slice_simulation.gd")
const RunPersistence = preload("res://src/persistence/run_persistence.gd")
const AccountRepository = preload("res://src/persistence/account_repository.gd")
const SaveCoordinator = preload("res://src/persistence/save_coordinator.gd")
const MetaShop = preload("res://src/economy/meta_shop.gd")
const TowerEconomy = preload("res://src/economy/tower_economy.gd")
const AppStateClass = preload("res://src/autoload/app_state.gd")
const VerticalSliceBot = preload("res://src/bot/vertical_slice_bot.gd")
const StrategicBotPlanner = preload("res://src/bot/strategic_bot_planner.gd")
const BotRuntime = preload("res://src/bot/bot_runtime.gd")
const EnemySpatialHash = preload("res://src/gameplay/enemy_spatial_hash.gd")
const MapPathfinder = preload("res://src/gameplay/map_pathfinder.gd")
const MapEditorState = preload("res://src/domain/map_editor_state.gd")
const GameLayout = preload("res://src/presentation/game_layout.gd")
const WorldGrid = preload("res://src/domain/world_grid.gd")
const RunScore = preload("res://src/gameplay/run_score.gd")
const RunRewards = preload("res://src/gameplay/run_rewards.gd")

var check_count := 0

func _init() -> void:
	var failures: PackedStringArray = DomainIds.validate_catalog()
	_expect_equal("terrain count", DomainIds.TERRAIN.size(), 4, failures)
	_expect_equal("map count", DomainIds.MAPS.size(), 5, failures)
	_expect_equal("mode count", DomainIds.MODES.size(), 5, failures)
	_expect_equal("tower count", DomainIds.TOWERS.size(), 9, failures)
	_expect_equal("enemy count", DomainIds.ENEMIES.size(), 11, failures)
	_test_domain_fixture(failures)
	_test_generated_catalog(failures)
	_test_transfer_fixture(failures)
	_test_import_planner(failures)
	_test_vertical_slice_scene(failures)
	_test_vertical_slice_simulation(failures)
	_test_tower_commands(failures)
	_test_economy_invariants(failures)
	_test_tower_capacity(failures)
	_test_run_persistence(failures)
	_test_combat_effects(failures)
	_test_crowd_control_limits(failures)
	_test_dynamic_obstacles(failures)
	_test_vertical_slice_bot(failures)
	_test_strategic_bot(failures)
	_test_enemy_spatial_hash(failures)
	_test_all_map_paths(failures)
	_test_modes_and_infinite_parity(failures)
	_test_difficulty_profiles(failures)
	_test_tower_upgrade_haskell_references(failures)
	_test_combat_haskell_references(failures)
	_test_enemy_time_haskell_references(failures)
	_test_boss_behaviors(failures)
	_test_account_repository(failures)
	_test_native_account_round_trip(failures)
	_test_save_coordinator(failures)
	_test_bot_checkpoint(failures)
	_test_meta_shop(failures)
	_test_shop_presentation(failures)
	_test_map_editor(failures)
	_test_bot_haskell_references(failures)
	_test_app_progression(failures)
	_test_run_score(failures)
	_test_run_rewards(failures)
	var rejected_future: Dictionary = {
		"schema": "immutable-towers-transfer",
		"version": 2,
		"source": {"game": "haskell-gloss", "exporter_version": 1},
		"accounts": [],
		"warnings": []
	}
	_expect_equal("future transfer rejected", TransferValidator.validate_document(rejected_future).is_empty(), false, failures)
	if failures.is_empty():
		print("Godot tests: PASS (" + str(check_count) + " checks)")
		quit(0)
	else:
		for failure: String in failures:
			push_error(failure)
		quit(1)

func _expect_equal(label: String, actual: Variant, expected: Variant, failures: PackedStringArray) -> void:
	check_count += 1
	if actual != expected:
		failures.append(label + ": expected " + str(expected) + ", got " + str(actual))

func _test_domain_fixture(failures: PackedStringArray) -> void:
	var fixture: Variant = _read_json_fixture("domain-catalog-v1.json", failures)
	if not fixture is Dictionary:
		return
	var catalog: Dictionary = fixture
	_expect_equal("fixture towers", catalog.get("towers", []), Array(DomainIds.TOWERS), failures)
	_expect_equal("fixture enemies", catalog.get("enemies", []), Array(DomainIds.ENEMIES), failures)

func _test_generated_catalog(failures: PackedStringArray) -> void:
	var catalog: Variant = DomainCatalog.new()
	catalog.load_default()
	_expect_equal("generated catalog valid", catalog.is_valid(), true, failures)
	_expect_equal("generated tower count", catalog.towers.size(), 9, failures)
	_expect_equal("generated enemy count", catalog.enemies.size(), 11, failures)
	_expect_equal("generated map count", catalog.maps.size(), 5, failures)
	_expect_equal("generated mode count", catalog.modes.size(), 5, failures)
	_expect_equal("sentinela price parity", catalog.tower("sentinela").get("price"), 44.0, failures)

func _test_transfer_fixture(failures: PackedStringArray) -> void:
	var fixture: Variant = _read_json_fixture("transfer-valid-v1.json", failures)
	if fixture == null:
		return
	_expect_equal("valid transfer", TransferValidator.validate_document(fixture).is_empty(), true, failures)

func _test_import_planner(failures: PackedStringArray) -> void:
	var fixture: Variant = _read_json_fixture("transfer-valid-v1.json", failures)
	if not fixture is Dictionary:
		return
	var transfer: Dictionary = fixture
	var fingerprint: String = TransferImportPlanner.fingerprint(transfer)
	_expect_equal("fingerprint deterministic", TransferImportPlanner.fingerprint(transfer), fingerprint, failures)
	var imported := PackedStringArray([fingerprint])
	_expect_equal("repeated import", TransferImportPlanner.plan_import(transfer, imported, false, false).get("status"), "already_imported", failures)
	_expect_equal("newer progress protected", TransferImportPlanner.plan_import(transfer, PackedStringArray(), true, false).get("status"), "confirmation_required", failures)
	_expect_equal("confirmed import ready", TransferImportPlanner.plan_import(transfer, PackedStringArray(), true, true).get("status"), "ready", failures)

func _read_json_fixture(file_name: String, failures: PackedStringArray) -> Variant:
	var path := ProjectSettings.globalize_path("res://../migration/fixtures/" + file_name)
	var file := FileAccess.open(path, FileAccess.READ)
	if file == null:
		failures.append("fixture missing: " + file_name)
		return null
	var parsed: Variant = JSON.parse_string(file.get_as_text())
	if parsed == null:
		failures.append("fixture invalid JSON: " + file_name)
	return parsed

func _test_vertical_slice_scene(failures: PackedStringArray) -> void:
	var packed_scene := load("res://scenes/game/vertical_slice.tscn") as PackedScene
	if packed_scene == null:
		failures.append("vertical slice scene failed to load")
		return
	var instance := packed_scene.instantiate()
	_expect_equal("vertical slice scene instantiates", instance != null, true, failures)
	instance.free()
	var boot_scene := load("res://scenes/boot/boot.tscn") as PackedScene
	var boot_instance: Node = boot_scene.instantiate() if boot_scene != null else null
	_expect_equal("boot scene instantiates", boot_instance != null, true, failures)
	if boot_instance != null:
		boot_instance.free()

func _test_vertical_slice_simulation(failures: PackedStringArray) -> void:
	var first := VerticalSliceSimulation.new()
	var second := VerticalSliceSimulation.new()
	for tick in 36000:
		first.update(1.0 / 60.0)
		second.update(1.0 / 60.0)
		if first.finished and second.finished:
			break
	_expect_equal("vertical slice finishes", first.finished, true, failures)
	_expect_equal("vertical slice deterministic result", first.victory, second.victory, failures)
	_expect_equal("vertical slice deterministic health", first.base_health, second.base_health, failures)
	_expect_equal("vertical slice deterministic credits", first.credits, second.credits, failures)

func _test_tower_commands(failures: PackedStringArray) -> void:
	var simulation := VerticalSliceSimulation.new()
	_expect_equal("build command succeeds", simulation.build_tower("sentinela", Vector2i(8, 6)), "", failures)
	_expect_equal("build command charges price", simulation.credits, 106, failures)
	_expect_equal("tower occupies one global cell", simulation.tower_occupied_cells(Vector2i(8, 6)).size(), 1, failures)
	_expect_equal("tower cell selects its tower", simulation.tower_at(Vector2i(8, 6)), 0, failures)
	_expect_equal("adjacent global cell remains independent", simulation.is_buildable(Vector2i(9, 6)), true, failures)
	_expect_equal("outside global grid is rejected", simulation.is_buildable(Vector2i(-1, 0)), false, failures)
	_expect_equal("build rejects occupied global cell", simulation.build_tower("sentinela", Vector2i(8, 6)), "A celula precisa de relva livre", failures)
	_expect_equal("build rejects enlarged path cell", simulation.build_tower("sentinela", Vector2i(5, 6)), "A celula precisa de relva livre", failures)
	simulation.credits = 1000
	var preview := simulation.upgrade_preview(0)
	_expect_equal("upgrade preview exposes exact next values", not preview.is_empty() and preview.get("damage") > simulation.towers[0].damage and preview.get("cost") == simulation.upgrade_cost(0), true, failures)
	_expect_equal("upgrade command succeeds", simulation.upgrade_tower(0), "", failures)
	var sale_value := simulation.sell_tower(0)
	_expect_equal("sell removes tower", sale_value > 0 and simulation.towers.is_empty(), true, failures)
	var defeated := simulation._create_enemy("basico", 1)
	defeated.health = 0.0
	simulation.enemies.append(defeated)
	simulation._remove_defeated()
	_expect_equal("enemy death emits visual feedback", simulation.visual_events.any(func(event: Dictionary) -> bool: return event.get("kind") == "death"), true, failures)
	var wave_simulation := VerticalSliceSimulation.new()
	for _tick in 90:
		wave_simulation.update(1.0 / 60.0)
	_expect_equal("wave progress keeps initial enemy total", wave_simulation.current_wave_total == wave_simulation.enemies.size() + wave_simulation.pending_enemies.size() and wave_simulation.current_wave_total > 0, true, failures)

func _test_economy_invariants(failures: PackedStringArray) -> void:
	var catalog := DomainCatalog.new()
	catalog.load_default()
	var immediate_sales_safe := true
	var upgraded_sales_safe := true
	var investment_tracking_safe := true
	var save_round_trip_safe := true
	for mode_id: String in DomainIds.MODES:
		for tower_id: String in DomainIds.TOWERS:
			for specialization: String in ["a", "b"]:
				var simulation := VerticalSliceSimulation.new(catalog, "planicie_serena", mode_id)
				simulation.credits = 1000000
				var initial_credits := simulation.credits
				var price := simulation.tower_price(tower_id)
				if not simulation.build_tower(tower_id, Vector2i(8, 6)).is_empty():
					immediate_sales_safe = false
					continue
				var tower := simulation.towers[0]
				investment_tracking_safe = investment_tracking_safe and tower.purchase_price_paid == price and tower.investment_paid == price
				if specialization == "a":
					var immediate_refund := simulation.sell_tower(0)
					immediate_sales_safe = immediate_sales_safe and immediate_refund == floori(float(price) * TowerEconomy.REFUND_RATE)
					immediate_sales_safe = immediate_sales_safe and simulation.credits <= initial_credits
					simulation = VerticalSliceSimulation.new(catalog, "planicie_serena", mode_id)
					simulation.credits = initial_credits
					simulation.build_tower(tower_id, Vector2i(8, 6))
					tower = simulation.towers[0]
				while tower.level < tower.max_level:
					var choice := specialization if tower.level >= 3 and tower.specialization.is_empty() else ""
					var cost := simulation.upgrade_cost(0, choice)
					var before_investment := tower.investment_paid
					if cost < 0 or not simulation.upgrade_tower(0, choice).is_empty():
						upgraded_sales_safe = false
						break
					investment_tracking_safe = investment_tracking_safe and tower.investment_paid == before_investment + cost
				var snapshot := simulation.snapshot()
				var restored := VerticalSliceSimulation.new(catalog, "planicie_serena", mode_id)
				save_round_trip_safe = save_round_trip_safe and restored.restore(snapshot).is_empty()
				save_round_trip_safe = save_round_trip_safe and restored.towers[0].investment_paid == tower.investment_paid
				var total_investment := tower.investment_paid
				var sale_value := simulation.sell_tower(0)
				upgraded_sales_safe = upgraded_sales_safe and sale_value <= total_investment
				upgraded_sales_safe = upgraded_sales_safe and simulation.credits <= initial_credits
	_expect_equal("immediate tower sales never create credits", immediate_sales_safe, true, failures)
	_expect_equal("upgraded tower sales never create credits", upgraded_sales_safe, true, failures)
	_expect_equal("tower investment tracks exact paid costs", investment_tracking_safe, true, failures)
	_expect_equal("tower investment survives save and load", save_round_trip_safe, true, failures)

func _test_tower_capacity(failures: PackedStringArray) -> void:
	var catalog := DomainCatalog.new()
	catalog.load_default()
	var expected_limits := {
		"history": 18,
		"infinite": 24,
		"challenge": 12,
		"boss": 16,
		"sandbox": 30
	}
	for mode_id: String in expected_limits:
		var mode_simulation := VerticalSliceSimulation.new(catalog, "planicie_serena", mode_id)
		_expect_equal(mode_id + " tower capacity", mode_simulation.tower_limit, expected_limits[mode_id], failures)
	var simulation := VerticalSliceSimulation.new(catalog, "planicie_serena", "challenge")
	simulation.credits = 100000
	for index in simulation.tower_limit:
		simulation.towers.append(VerticalSliceSimulation.TowerState.new("sentinela", Vector2(20.5 + index, 20.5), catalog.tower("sentinela")))
	_expect_equal("tower capacity disables buildable cells", simulation.is_buildable(Vector2i(8, 6)), false, failures)
	_expect_equal("tower capacity rejects construction", simulation.build_tower("sentinela", Vector2i(8, 6)), "Limite de 12 torres atingido", failures)
	_expect_equal("bot never proposes a build at capacity", VerticalSliceBot.decide(simulation).get("kind") != "build", true, failures)
	simulation.sell_tower(simulation.towers.size() - 1)
	_expect_equal("selling a tower frees capacity", simulation.has_tower_capacity() and simulation.build_tower("sentinela", Vector2i(8, 6)).is_empty(), true, failures)
	var legacy_snapshot := simulation.snapshot()
	legacy_snapshot["towers"].append(legacy_snapshot["towers"][0].duplicate(true))
	var restored := VerticalSliceSimulation.new(catalog, "planicie_serena", "challenge")
	_expect_equal("over-cap legacy runs keep every tower", restored.restore(legacy_snapshot).is_empty() and restored.towers.size() == 13, true, failures)
	_expect_equal("over-cap legacy runs cannot add towers", restored.has_tower_capacity(), false, failures)
	_expect_equal("global game tile is three old cells wide", GameLayout.TILE_SIZE, 66.0, failures)
	_expect_equal("global grid width", GameLayout.MAP_WIDTH, 12, failures)
	_expect_equal("global grid height", GameLayout.MAP_HEIGHT, 11, failures)
	_expect_equal("global map keeps approximately the original screen area", GameLayout.MAP_PIXEL_SIZE, Vector2(792, 726), failures)
	_expect_equal("catalog distances convert to global cells", is_equal_approx(simulation.world_distance(5.3), 5.3 / 3.0), true, failures)
	var every_map_reaches_sandbox_capacity := true
	for map_id: String in DomainIds.MAPS:
		var map_simulation := VerticalSliceSimulation.new(catalog, map_id, "sandbox")
		map_simulation.credits = 100000
		for y in map_simulation.map_grid.size():
			for x in map_simulation.map_grid[y].size():
				if map_simulation.is_buildable(Vector2i(x, y)):
					map_simulation.build_tower("sentinela", Vector2i(x, y))
				if not map_simulation.has_tower_capacity():
					break
			if not map_simulation.has_tower_capacity():
				break
		every_map_reaches_sandbox_capacity = every_map_reaches_sandbox_capacity and map_simulation.towers.size() == map_simulation.tower_limit
	_expect_equal("all official maps support sandbox capacity on global cells", every_map_reaches_sandbox_capacity, true, failures)

func _test_run_persistence(failures: PackedStringArray) -> void:
	var path := "user://test-vertical-slice-run-v1.json"
	for suffix in ["", ".tmp", ".bak"]:
		if FileAccess.file_exists(path + suffix):
			DirAccess.remove_absolute(path + suffix)
	var original := VerticalSliceSimulation.new()
	original.build_tower("sentinela", Vector2i(8, 6))
	for tick in 180:
		original.update(1.0 / 60.0)
	_expect_equal("run save succeeds", RunPersistence.save_snapshot(original.snapshot(), path), "", failures)
	var loaded: Variant = RunPersistence.load_snapshot(path)
	_expect_equal("run save loads", loaded is Dictionary, true, failures)
	var restored := VerticalSliceSimulation.new()
	_expect_equal("run restore validates", restored.restore(loaded).is_empty(), true, failures)
	_expect_equal("run credits round trip", restored.credits, original.credits, failures)
	_expect_equal("run towers round trip", restored.towers.size(), original.towers.size(), failures)
	RunPersistence.save_snapshot(original.snapshot(), path)
	var corrupt_file := FileAccess.open(path, FileAccess.WRITE)
	corrupt_file.store_string("{invalid")
	corrupt_file.close()
	_expect_equal("run backup recovers", RunPersistence.load_snapshot(path) is Dictionary, true, failures)
	var future := original.snapshot()
	future["version"] = 2
	_expect_equal("future run rejected", restored.restore(future).is_empty(), false, failures)
	var legacy := original.snapshot()
	legacy.erase("pending_enemies")
	legacy["pending_classes"] = ["basico", "rapido"]
	_expect_equal("legacy pending classes restore", restored.restore(legacy).is_empty() and restored.pending_enemies.size() == 2, true, failures)
	var fine_grid_save := original.snapshot()
	fine_grid_save.erase("world_grid_scale")
	fine_grid_save["towers"][0]["position"] = {"x": 8.5, "y": 6.5}
	var migrated := VerticalSliceSimulation.new()
	_expect_equal("old fine-grid run migrates without losing towers", migrated.restore(fine_grid_save).is_empty() and migrated.towers.size() == 1, true, failures)
	_expect_equal("old tower coordinates migrate to one global cell", migrated.towers[0].position, Vector2(2.5, 2.5), failures)
	for suffix in ["", ".tmp", ".bak"]:
		if FileAccess.file_exists(path + suffix):
			DirAccess.remove_absolute(path + suffix)

func _test_combat_effects(failures: PackedStringArray) -> void:
	var simulation := VerticalSliceSimulation.new()
	var frozen := simulation._create_enemy("basico", 1)
	simulation._apply_projectile(frozen, "ice", 1.0)
	var frozen_position := frozen.position
	simulation.enemies = [frozen]
	simulation._update_enemies(0.5)
	_expect_equal("ice slows without stopping movement", frozen.position.x > frozen_position.x and frozen.position.x < frozen_position.x + frozen.speed * 0.5, true, failures)
	var burning := simulation._create_enemy("basico", 1)
	simulation._apply_projectile(burning, "fire", 2.0)
	var burning_health := burning.health
	simulation.enemies = [burning]
	simulation._update_enemies(1.0)
	_expect_equal("fire deals damage over time", burning.health < burning_health, true, failures)
	var afraid := simulation._create_enemy("basico", 1)
	afraid.position = Vector2(5.5, 2.5)
	simulation._apply_projectile(afraid, "fear", 1.0)
	simulation.enemies = [afraid]
	simulation._update_enemies(0.5)
	_expect_equal("fear moves backwards", afraid.position.x < 5.5, true, failures)
	var normal := simulation._create_enemy("basico", 1)
	var resin := simulation._create_enemy("basico", 1)
	simulation._apply_projectile(resin, "resin", 1.0)
	simulation.enemies = [normal, resin]
	simulation._update_enemies(0.5)
	_expect_equal("resin slows movement", resin.position.x < normal.position.x, true, failures)
	simulation = VerticalSliceSimulation.new()
	simulation.credits = 1000
	simulation.build_tower("sentinela", Vector2i(8, 6))
	var basic := simulation._create_enemy("basico", 1)
	var armored := simulation._create_enemy("blindado", 1)
	var tower := simulation.towers[0]
	var basic_before := basic.health
	var armored_before := armored.health
	simulation._resolve_tower_hit(tower, basic)
	simulation._resolve_tower_hit(tower, armored)
	_expect_equal("armor reduces direct damage", armored_before - armored.health < basic_before - basic.health, true, failures)
	simulation = VerticalSliceSimulation.new()
	simulation.credits = 1000
	simulation.build_tower("glaciar", Vector2i(8, 6))
	var first_target := simulation._create_enemy("basico", 1)
	var second_target := simulation._create_enemy("rapido", 1)
	first_target.position = Vector2(8.5, 5.5)
	second_target.position = Vector2(9.5, 5.5)
	simulation.enemies = [first_target, second_target]
	simulation._update_towers(0.0)
	_expect_equal("burst hits two targets", simulation.shots.size(), 2, failures)
	var synergy_target := simulation._create_enemy("basico", 1)
	simulation._apply_projectile(synergy_target, "ice", 2.0)
	simulation._apply_projectile(synergy_target, "fire", 2.0)
	_expect_equal("fire and ice cancel", not synergy_target.effects.has("fire") and not synergy_target.effects.has("ice"), true, failures)

func _test_crowd_control_limits(failures: PackedStringArray) -> void:
	var simulation := VerticalSliceSimulation.new()
	var normal := simulation._create_enemy("basico", 1)
	var elite := simulation._create_enemy("elite", 1)
	var boss := simulation._create_enemy("boss_guardiao", 1)
	for _hit in 100:
		simulation._apply_projectile(normal, "electric", 4.0)
		simulation._apply_projectile(elite, "electric", 4.0)
		simulation._apply_projectile(boss, "electric", 4.0)
	_expect_equal("normal hard control has a continuous cap", float(normal.effects.get("electric", 0.0)) <= 1.5, true, failures)
	_expect_equal("elite hard control has stronger tenacity", float(elite.effects.get("electric", 0.0)) <= 1.0, true, failures)
	_expect_equal("boss hard control has maximum tenacity", float(boss.effects.get("electric", 0.0)) <= 0.5, true, failures)
	var normal_fear := simulation._create_enemy("basico", 1)
	normal_fear.position = simulation.path[3]
	normal_fear.segment = 3
	var boss_fear := simulation._create_enemy("boss_acelerador", 1)
	boss_fear.position = simulation.path[3]
	boss_fear.segment = 3
	simulation._apply_projectile(normal_fear, "fear", 4.0)
	simulation._apply_projectile(boss_fear, "fear", 4.0)
	var normal_before := normal_fear.position
	var boss_before := boss_fear.position
	simulation.enemies = [normal_fear, boss_fear]
	simulation._update_enemies(0.25)
	_expect_equal("fear can retreat normal enemies", normal_fear.position.distance_to(simulation.path[0]) < normal_before.distance_to(simulation.path[0]), true, failures)
	_expect_equal("fear never retreats bosses", boss_fear.position.distance_to(simulation.path[0]) > boss_before.distance_to(simulation.path[0]), true, failures)
	var frozen := simulation._create_enemy("basico", 1)
	var unfrozen := simulation._create_enemy("basico", 1)
	simulation._apply_projectile(frozen, "ice", 8.0)
	simulation.enemies = [frozen, unfrozen]
	var frozen_before := frozen.position
	var unfrozen_before := unfrozen.position
	simulation._update_enemies(0.5)
	var frozen_distance := frozen.position.distance_to(frozen_before)
	var unfrozen_distance := unfrozen.position.distance_to(unfrozen_before)
	_expect_equal("ice keeps a positive movement floor", frozen_distance > 0.0 and frozen_distance < unfrozen_distance, true, failures)
	simulation._apply_projectile(frozen, "electric", 1.0)
	var snapshot := simulation.snapshot()
	var restored := VerticalSliceSimulation.new()
	_expect_equal(
		"crowd control state survives save and load",
		restored.restore(snapshot).is_empty()
			and is_equal_approx(restored.enemies[0].hard_cc_exposure, frozen.hard_cc_exposure)
			and is_equal_approx(restored.enemies[0].cc_window_remaining, frozen.cc_window_remaining),
		true,
		failures
	)

func _test_dynamic_obstacles(failures: PackedStringArray) -> void:
	var simulation := VerticalSliceSimulation.new()
	var obstacle_cell := Vector2i(5, 2)
	_expect_equal("dynamic obstacle can be placed on route", simulation.place_obstacle(obstacle_cell), "", failures)
	_expect_equal("dynamic obstacle protects portal", simulation.place_obstacle(Vector2i(simulation.path[0])), "Portal e base nao podem ser bloqueados", failures)
	var slowed := simulation._create_enemy("basico", 1)
	slowed.position = Vector2(5.5, 2.5)
	slowed.segment = _nearest_path_segment(simulation.path, slowed.position)
	simulation.enemies = [slowed]
	var normal_simulation := VerticalSliceSimulation.new()
	var normal := normal_simulation._create_enemy("basico", 1)
	normal.position = Vector2(5.5, 2.5)
	normal.segment = _nearest_path_segment(normal_simulation.path, normal.position)
	normal_simulation.enemies = [normal]
	simulation._update_enemies(0.5)
	normal_simulation._update_enemies(0.5)
	_expect_equal("dynamic obstacle slows enemies", slowed.position.distance_to(Vector2(5.5, 2.5)) < normal.position.distance_to(Vector2(5.5, 2.5)), true, failures)
	var restored := VerticalSliceSimulation.new()
	_expect_equal("dynamic obstacles survive run restore", restored.restore(simulation.snapshot()).is_empty() and restored.obstacles.has(obstacle_cell), true, failures)

func _test_vertical_slice_bot(failures: PackedStringArray) -> void:
	var simulation := VerticalSliceSimulation.new()
	var first: Dictionary = VerticalSliceBot.decide(simulation)
	var second: Dictionary = VerticalSliceBot.decide(simulation)
	_expect_equal("bot decision deterministic kind", first.get("kind"), second.get("kind"), failures)
	_expect_equal("bot decision deterministic cell", first.get("cell"), second.get("cell"), failures)
	_expect_equal("bot chooses legal build", first.get("kind"), "build", failures)
	_expect_equal("bot applies shared command", VerticalSliceBot.apply(simulation, first), "", failures)
	_expect_equal("bot build preserves economy", simulation.towers.size() == 1 and simulation.credits >= 0, true, failures)

func _test_strategic_bot(failures: PackedStringArray) -> void:
	var catalog := DomainCatalog.new()
	catalog.load_default()
	var all_towers := DomainIds.TOWERS.duplicate()
	var simulation := VerticalSliceSimulation.new(catalog, "planicie_serena", "sandbox", all_towers)
	simulation.credits = 999
	var first := VerticalSliceBot.decide(simulation)
	var second := VerticalSliceBot.decide(simulation)
	_expect_equal("strategic bot remains deterministic", first.get("kind") == second.get("kind") and first.get("cell") == second.get("cell") and first.get("tower_id") == second.get("tower_id"), true, failures)
	_expect_equal("strategic bot chooses an unlocked legal action", first.get("kind") == "build" and first.get("tower_id") in all_towers and simulation.is_buildable(first.get("cell", Vector2i(-1, -1))), true, failures)
	_expect_equal("strategic bot explains score components", first.get("breakdown") is Dictionary and not first.get("breakdown", {}).is_empty(), true, failures)
	_expect_equal("strategic bot includes a bounded rollout projection", first.get("breakdown", {}).has("previsao"), true, failures)
	_expect_equal("strategic bot limits alternatives", first.get("alternatives", []).size() <= 2, true, failures)
	var planner := StrategicBotPlanner.new()
	var planned := planner.decide(simulation)
	var outcome := BotRuntime.execute(simulation, planned, func() -> String: return "")
	planner.record_outcome(simulation, planned, outcome)
	_expect_equal("strategic planner remembers successful tower actions", not planner.memory.get("recent_tower_ids", PackedStringArray()).is_empty(), true, failures)
	var planner_snapshot := planner.snapshot()
	var restored_planner := StrategicBotPlanner.new()
	restored_planner.restore(planner_snapshot)
	_expect_equal("strategic planner memory round trips", restored_planner.snapshot(), planner_snapshot, failures)
	var restricted := VerticalSliceSimulation.new(catalog, "planicie_serena", "history", PackedStringArray(["sentinela"]))
	_expect_equal("restricted bot explains limited arsenal", "apenas SENTINELA" in str(VerticalSliceBot.decide(restricted).get("reason", "")), true, failures)

func _test_enemy_spatial_hash(failures: PackedStringArray) -> void:
	var simulation := VerticalSliceSimulation.new()
	var near_enemy := simulation._create_enemy("basico", 1)
	var far_enemy := simulation._create_enemy("rapido", 1)
	near_enemy.position = Vector2(2.0, 2.0)
	far_enemy.position = Vector2(20.0, 20.0)
	var spatial := EnemySpatialHash.new(4.0)
	spatial.rebuild([near_enemy, far_enemy])
	var nearby := spatial.query(Vector2(2.0, 2.0), 3.0)
	_expect_equal("spatial query contains nearby enemy", near_enemy in nearby, true, failures)
	_expect_equal("spatial query excludes distant enemy", far_enemy in nearby, false, failures)

func _test_all_map_paths(failures: PackedStringArray) -> void:
	var catalog: Variant = DomainCatalog.new()
	catalog.load_default()
	var all_connected := true
	var endpoints_match := true
	var routes_only := true
	var dimensions_match := true
	var cropped_source_tail_is_empty := true
	var path_scale_is_coherent := true
	for map_id: String in DomainIds.MAPS:
		var source_grid: Array = catalog.map_data(map_id).get("grid", [])
		var source_map: Dictionary = catalog.map_data(map_id)
		var source_portal: Dictionary = source_map.get("portal", {}).get("position", {})
		var source_base: Dictionary = source_map.get("base", {}).get("position", {})
		var source_path := MapPathfinder.find_path(
			source_grid,
			Vector2i(floori(float(source_portal.get("x", 0.0))), floori(float(source_portal.get("y", 0.0)))),
			Vector2i(floori(float(source_base.get("x", 0.0))), floori(float(source_base.get("y", 0.0))))
		)
		if source_grid.size() > WorldGrid.HEIGHT * WorldGrid.SOURCE_CELLS_PER_CELL:
			for terrain: Variant in source_grid[WorldGrid.HEIGHT * WorldGrid.SOURCE_CELLS_PER_CELL]:
				cropped_source_tail_is_empty = cropped_source_tail_is_empty and terrain == "grass"
		var simulation := VerticalSliceSimulation.new(catalog, map_id)
		var grid: Array = simulation.map_grid
		var route: Array[Vector2] = simulation.path
		var path_ratio := float(maxi(1, route.size() - 1)) / float(maxi(1, source_path.size() - 1))
		path_scale_is_coherent = path_scale_is_coherent and path_ratio >= 0.25 and path_ratio <= 0.45
		var start := Vector2i(route[0]) if not route.is_empty() else Vector2i(-1, -1)
		var goal := Vector2i(route[route.size() - 1]) if not route.is_empty() else Vector2i(-1, -1)
		all_connected = all_connected and not route.is_empty()
		dimensions_match = dimensions_match and grid.size() == WorldGrid.HEIGHT and not grid.is_empty() and grid[0].size() == WorldGrid.WIDTH
		if not route.is_empty():
			endpoints_match = endpoints_match and Vector2i(route[0]) == start and Vector2i(route[route.size() - 1]) == goal
			for point: Vector2 in route:
				routes_only = routes_only and MapPathfinder._is_route_cell(grid, Vector2i(point))
	_expect_equal("all five maps connected", all_connected, true, failures)
	_expect_equal("all map endpoints match", endpoints_match, true, failures)
	_expect_equal("all paths avoid water", routes_only, true, failures)
	_expect_equal("all maps use the 12x11 global grid", dimensions_match, true, failures)
	_expect_equal("cropped source tail is decorative grass on every map", cropped_source_tail_is_empty, true, failures)
	_expect_equal("all route lengths scale coherently", path_scale_is_coherent, true, failures)

func _test_modes_and_infinite_parity(failures: PackedStringArray) -> void:
	var catalog: Variant = DomainCatalog.new()
	catalog.load_default()
	_expect_equal("history exports ten waves", catalog.mode("history").get("waves", []).size(), 10, failures)
	var expected := {
		"history": [80, 150, 10],
		"infinite": [80, 165, 1],
		"challenge": [60, 125, 3],
		"boss": [110, 180, 3],
		"sandbox": [80, 999, 1]
	}
	var setup_matches := true
	for mode_id: String in expected:
		var simulation := VerticalSliceSimulation.new(catalog, "planicie_serena", mode_id)
		var values: Array = expected[mode_id]
		setup_matches = setup_matches and int(simulation.base_health) == values[0]
		setup_matches = setup_matches and simulation.credits == values[1]
		setup_matches = setup_matches and simulation.wave_definitions.size() == values[2]
	_expect_equal("all mode starting states match Haskell", setup_matches, true, failures)
	var infinite := VerticalSliceSimulation.new(catalog, "planicie_serena", "infinite")
	var references: Array = catalog.mode("infinite").get("infinite_reference_waves", [])
	_expect_equal("infinite has eighteen Haskell references", references.size(), 18, failures)
	for wave_number in range(1, references.size() + 1):
		var generated: Dictionary = infinite._generate_infinite_wave(wave_number)
		var reference: Dictionary = references[wave_number - 1]
		_expect_equal("infinite wave " + str(wave_number) + " matches Haskell", _wave_difference(generated, reference), "", failures)

func _test_difficulty_profiles(failures: PackedStringArray) -> void:
	var catalog := DomainCatalog.new()
	catalog.load_default()
	var reference := VerticalSliceSimulation.new(catalog, "planicie_serena", "history")
	var stage_one := VerticalSliceSimulation.new(catalog, "planicie_serena", "history", DomainIds.TOWERS, {}, {
		"campaign_profile": true,
		"chapter": 1,
		"stage": 1
	})
	_expect_equal("campaign stage one preserves the reference waves", stage_one.wave_definitions, reference.wave_definitions, failures)
	var stage_signatures: Dictionary = {}
	for stage in range(1, 6):
		var profiled := VerticalSliceSimulation.new(catalog, "planicie_serena", "history", DomainIds.TOWERS, {}, {
			"campaign_profile": true,
			"chapter": 1,
			"stage": stage
		})
		stage_signatures[JSON.stringify(profiled.wave_definitions)] = true
	_expect_equal("five campaign stages have distinct wave profiles", stage_signatures.size(), 5, failures)
	var chapter_two := VerticalSliceSimulation.new(catalog, "planicie_serena", "history", DomainIds.TOWERS, {}, {
		"campaign_profile": true,
		"chapter": 2,
		"stage": 1
	})
	var stage_one_health := float(stage_one.wave_definitions[0].get("enemies", [])[0].get("health", 0.0))
	var chapter_two_health := float(chapter_two.wave_definitions[0].get("enemies", [])[0].get("health", 0.0))
	_expect_equal("later chapters scale enemy health", chapter_two_health > stage_one_health, true, failures)
	var restored := VerticalSliceSimulation.new()
	_expect_equal(
		"campaign profile survives run restore",
		restored.restore(chapter_two.snapshot()).is_empty()
			and restored.run_context == chapter_two.run_context
			and restored.difficulty_profile == chapter_two.difficulty_profile,
		true,
		failures
	)

func _test_tower_upgrade_haskell_references(failures: PackedStringArray) -> void:
	var catalog := DomainCatalog.new()
	catalog.load_default()
	var references: Array = catalog.document.get("tower_upgrade_references", [])
	_expect_equal("eighteen tower specialization paths exported", references.size(), 18, failures)
	var state_count := 0
	var stats_match := true
	var costs_match := true
	var sales_safe := true
	for reference: Dictionary in references:
		var tower_id := str(reference.get("tower_id"))
		var path_id := str(reference.get("specialization_path"))
		var simulation := VerticalSliceSimulation.new(catalog)
		simulation.towers = [VerticalSliceSimulation.TowerState.new(tower_id, Vector2(8.5, 6.5), catalog.tower(tower_id))]
		simulation.credits = 1000000
		var states: Array = reference.get("states", [])
		state_count += states.size()
		for state_index in states.size():
			var state: Dictionary = states[state_index]
			var tower: Variant = simulation.towers[0]
			var stats: Dictionary = state.get("stats", {})
			var duration: Dictionary = stats.get("projectile", {}).get("duration", {})
			var expected_specialization := "" if state.get("specialization") == null else str(state.get("specialization"))
			stats_match = stats_match and tower.level == int(state.get("level"))
			stats_match = stats_match and tower.specialization == expected_specialization
			stats_match = stats_match and absf(tower.damage - float(stats.get("damage"))) < 0.0002
			stats_match = stats_match and absf(tower.range_cells - float(stats.get("range"))) < 0.0002
			stats_match = stats_match and absf(tower.cycle - float(stats.get("cycle"))) < 0.0002
			stats_match = stats_match and tower.burst == int(stats.get("burst"))
			stats_match = stats_match and absf(tower.projectile_duration - float(duration.get("seconds", tower.projectile_duration))) < 0.0002
			var expected_cost := -1 if state.get("upgrade_cost") == null else int(state.get("upgrade_cost"))
			var specialization_choice := path_id if tower.level >= 3 and tower.specialization.is_empty() else ""
			costs_match = costs_match and simulation.upgrade_cost(0, specialization_choice) == expected_cost
			sales_safe = sales_safe and TowerEconomy.sale_value(tower) <= tower.investment_paid
			if state_index < states.size() - 1:
				stats_match = stats_match and simulation.upgrade_tower(0, specialization_choice).is_empty()
	_expect_equal("all tower tier states exported", state_count, 98, failures)
	_expect_equal("tower stats match Haskell at every tier", stats_match, true, failures)
	_expect_equal("tower upgrade costs match Haskell", costs_match, true, failures)
	_expect_equal("tower sale values never exceed paid investment", sales_safe, true, failures)
	var prices_match := true
	for mode_reference: Dictionary in catalog.document.get("tower_price_references", []):
		var price_simulation := VerticalSliceSimulation.new(catalog, "planicie_serena", str(mode_reference.get("mode_id")))
		for price: Dictionary in mode_reference.get("prices", []):
			prices_match = prices_match and price_simulation.tower_price(str(price.get("tower_id"))) == int(price.get("price"))
	_expect_equal("all mode tower prices match Haskell", prices_match, true, failures)

func _test_combat_haskell_references(failures: PackedStringArray) -> void:
	var catalog := DomainCatalog.new()
	catalog.load_default()
	var references: Array = catalog.document.get("combat_hit_references", [])
	_expect_equal("all tower enemy hit pairs exported", references.size(), 99, failures)
	var health_mismatch := ""
	var effects_match := true
	var pairs := {}
	for reference: Dictionary in references:
		var tower_id := str(reference.get("tower_id"))
		var enemy_id := str(reference.get("enemy_id"))
		pairs[tower_id + ":" + enemy_id] = true
		var simulation := VerticalSliceSimulation.new(catalog)
		var tower := VerticalSliceSimulation.TowerState.new(tower_id, Vector2(8.5, 6.5), catalog.tower(tower_id))
		var enemy := simulation._create_enemy(enemy_id, int(reference.get("level", 5)))
		enemy.position = Vector2(8.5, 6.5)
		simulation.towers = [tower]
		simulation.enemies = [enemy]
		simulation.enemy_spatial.rebuild(simulation.enemies)
		simulation._resolve_tower_hit(tower, enemy)
		var expected_health := float(reference.get("health_after"))
		if health_mismatch.is_empty() and absf(enemy.health - expected_health) >= 0.0005:
			health_mismatch = tower_id + " x " + enemy_id + ": expected " + str(expected_health) + ", got " + str(enemy.health)
		var expected_effects := {}
		for effect: Dictionary in reference.get("effects", []):
			var duration: Dictionary = effect.get("duration", {})
			expected_effects[str(effect.get("type_id"))] = 1000000.0 if duration.get("kind") == "infinite" else float(duration.get("seconds", 0.0))
		effects_match = effects_match and enemy.effects.size() == expected_effects.size()
		for effect_id: String in expected_effects:
			effects_match = effects_match and enemy.effects.has(effect_id)
			effects_match = effects_match and float(enemy.effects.get(effect_id, 0.0)) > 0.0
			effects_match = effects_match and float(enemy.effects.get(effect_id, 0.0)) <= float(expected_effects[effect_id]) + 0.0005
	_expect_equal("combat reference pairs are unique", pairs.size(), 99, failures)
	_expect_equal("tower damage and defenses match Haskell", health_mismatch, "", failures)
	_expect_equal("projectile effects preserve Haskell types with bounded duration", effects_match, true, failures)
	var sequences: Array = catalog.document.get("combat_sequence_references", [])
	_expect_equal("ordered projectile sequences exported", sequences.size(), 8, failures)
	var sequence_health_mismatch := ""
	var sequence_effects_match := true
	for reference: Dictionary in sequences:
		var simulation := VerticalSliceSimulation.new(catalog)
		var enemy := simulation._create_enemy(str(reference.get("enemy_id", "basico")), int(reference.get("level", 8)))
		enemy.position = Vector2(8.5, 6.5)
		simulation.enemies = [enemy]
		for tower_id: String in reference.get("tower_ids", []):
			var tower := VerticalSliceSimulation.TowerState.new(tower_id, Vector2(8.5, 6.5), catalog.tower(tower_id))
			simulation.towers = [tower]
			simulation.enemy_spatial.rebuild(simulation.enemies)
			simulation._resolve_tower_hit(tower, enemy)
		var expected_health := float(reference.get("health_after"))
		if sequence_health_mismatch.is_empty() and absf(enemy.health - expected_health) >= 0.0005:
			sequence_health_mismatch = str(reference.get("tower_ids")) + ": expected " + str(expected_health) + ", got " + str(enemy.health)
		var expected_effects := {}
		for effect: Dictionary in reference.get("effects", []):
			var duration: Dictionary = effect.get("duration", {})
			expected_effects[str(effect.get("type_id"))] = 1000000.0 if duration.get("kind") == "infinite" else float(duration.get("seconds", 0.0))
		sequence_effects_match = sequence_effects_match and enemy.effects.size() == expected_effects.size()
		for effect_id: String in expected_effects:
			sequence_effects_match = sequence_effects_match and enemy.effects.has(effect_id)
			sequence_effects_match = sequence_effects_match and float(enemy.effects.get(effect_id, 0.0)) > 0.0
			sequence_effects_match = sequence_effects_match and float(enemy.effects.get(effect_id, 0.0)) <= float(expected_effects[effect_id]) + 0.0005
	_expect_equal("ordered synergy damage matches Haskell", sequence_health_mismatch, "", failures)
	_expect_equal("ordered synergy effects remain bounded", sequence_effects_match, true, failures)

func _test_enemy_time_haskell_references(failures: PackedStringArray) -> void:
	var catalog := DomainCatalog.new()
	catalog.load_default()
	var update_references: Array = catalog.document.get("enemy_update_references", [])
	_expect_equal("enemy time references exported", update_references.size(), 7, failures)
	var health_mismatch := ""
	var position_mismatch := ""
	var effects_mismatch := ""
	for reference: Dictionary in update_references:
		var simulation := VerticalSliceSimulation.new(catalog)
		var enemy := simulation._create_enemy(str(reference.get("enemy_id")), int(reference.get("level", 1)))
		var source_start := _vector_from_json(reference.get("start_position", {}))
		var source_expected := _vector_from_json(reference.get("position_after", {}))
		enemy.position = simulation.path[0]
		enemy.segment = _nearest_path_segment(simulation.path, enemy.position)
		var world_start := enemy.position
		for effect: Dictionary in reference.get("effects_before", []):
			var duration: Dictionary = effect.get("duration", {})
			enemy.effects[str(effect.get("type_id"))] = 1000000.0 if duration.get("kind") == "infinite" else float(duration.get("seconds", 0.0))
		simulation.enemies = [enemy]
		simulation._update_enemies(float(reference.get("delta")))
		var scenario_id := str(reference.get("id"))
		if health_mismatch.is_empty() and absf(enemy.health - float(reference.get("health_after"))) >= 0.0005:
			health_mismatch = scenario_id + ": expected " + str(reference.get("health_after")) + ", got " + str(enemy.health)
		var expected_distance := source_start.distance_to(source_expected) * WorldGrid.DISTANCE_SCALE
		var actual_distance := world_start.distance_to(enemy.position)
		var has_reworked_control := false
		for effect: Dictionary in reference.get("effects_before", []):
			has_reworked_control = has_reworked_control or str(effect.get("type_id")) in ["ice", "electric", "fear", "resin"]
		if not has_reworked_control and position_mismatch.is_empty() and absf(actual_distance - expected_distance) >= 0.0005:
			position_mismatch = scenario_id + ": expected scaled movement " + str(expected_distance) + ", got " + str(actual_distance)
		var expected_effects := {}
		for effect: Dictionary in reference.get("effects_after", []):
			var duration: Dictionary = effect.get("duration", {})
			expected_effects[str(effect.get("type_id"))] = 1000000.0 if duration.get("kind") == "infinite" else float(duration.get("seconds", 0.0))
		if effects_mismatch.is_empty() and not _effect_dictionaries_match(enemy.effects, expected_effects):
			effects_mismatch = scenario_id + ": expected " + str(expected_effects) + ", got " + str(enemy.effects)
	_expect_equal("enemy time health matches Haskell", health_mismatch, "", failures)
	_expect_equal("non-control enemy movement matches Haskell", position_mismatch, "", failures)
	_expect_equal("enemy effect durations match Haskell", effects_mismatch, "", failures)

	var arrival_references: Array = catalog.document.get("base_arrival_references", [])
	_expect_equal("base arrival references exported", arrival_references.size(), 5, failures)
	var arrival_mismatch := ""
	var survivor_position_mismatch := ""
	for reference: Dictionary in arrival_references:
		var simulation := VerticalSliceSimulation.new(catalog)
		var enemy := simulation._create_enemy(str(reference.get("enemy_id")), int(reference.get("level", 1)))
		var source_start := _vector_from_json(reference.get("start_position", {}))
		var source_base := Vector2(35.5, 22.5)
		var base_position := simulation.path[simulation.path.size() - 1]
		var final_direction := (base_position - simulation.path[simulation.path.size() - 2]).normalized()
		enemy.position = base_position - final_direction * source_start.distance_to(source_base) * WorldGrid.DISTANCE_SCALE
		enemy.segment = simulation.path.size() - 2
		simulation.base_health = float(reference.get("base_health_before"))
		simulation.enemies = [enemy]
		simulation._update_enemies(float(reference.get("delta")))
		var scenario_id := str(reference.get("id"))
		if arrival_mismatch.is_empty() and (simulation.enemies.size() != int(reference.get("remaining_enemies")) or absf(simulation.base_health - float(reference.get("base_health_after"))) >= 0.0005):
			arrival_mismatch = scenario_id + ": expected health/enemies " + str(reference.get("base_health_after")) + "/" + str(reference.get("remaining_enemies")) + ", got " + str(simulation.base_health) + "/" + str(simulation.enemies.size())
		var expected_position: Variant = reference.get("enemy_position_after")
		if survivor_position_mismatch.is_empty() and expected_position is Dictionary and not simulation.enemies.is_empty():
			var source_expected := _vector_from_json(expected_position)
			var expected_vector := base_position - final_direction * source_expected.distance_to(source_base) * WorldGrid.DISTANCE_SCALE
			if simulation.enemies[0].position.distance_to(expected_vector) >= 0.0005:
				survivor_position_mismatch = scenario_id + ": expected " + str(expected_vector) + ", got " + str(simulation.enemies[0].position)
	_expect_equal("base arrival and damage match Haskell", arrival_mismatch, "", failures)
	_expect_equal("pre-base movement matches Haskell", survivor_position_mismatch, "", failures)

func _nearest_path_segment(path: Array[Vector2], position: Vector2) -> int:
	var nearest := 0
	var nearest_distance := INF
	for index in path.size():
		var distance := path[index].distance_squared_to(position)
		if distance < nearest_distance:
			nearest = index
			nearest_distance = distance
	return nearest

func _vector_from_json(value: Variant) -> Vector2:
	if not value is Dictionary:
		return Vector2.ZERO
	return Vector2(float(value.get("x", 0.0)), float(value.get("y", 0.0)))

func _effect_dictionaries_match(actual: Dictionary, expected: Dictionary) -> bool:
	if actual.size() != expected.size():
		return false
	for effect_id: String in expected:
		if not actual.has(effect_id) or absf(float(actual.get(effect_id)) - float(expected.get(effect_id))) >= 0.0005:
			return false
	return true

func _wave_difference(left: Dictionary, right: Dictionary) -> String:
	if not is_equal_approx(float(left.get("cycle", 0.0)), float(right.get("cycle", 0.0))):
		return "cycle " + str(left.get("cycle")) + " != " + str(right.get("cycle"))
	var left_enemies: Array = left.get("enemies", [])
	var right_enemies: Array = right.get("enemies", [])
	if left_enemies.size() != right_enemies.size():
		return "enemy count " + str(left_enemies.size()) + " != " + str(right_enemies.size())
	for index in left_enemies.size():
		var a: Dictionary = left_enemies[index]
		var b: Dictionary = right_enemies[index]
		if a.get("class_id") != b.get("class_id"):
			return "enemy " + str(index) + " class " + str(a.get("class_id")) + " != " + str(b.get("class_id"))
		if int(a.get("loot")) != int(b.get("loot")):
			return "enemy " + str(index) + " loot " + str(a.get("loot")) + " != " + str(b.get("loot"))
		for field in ["health", "speed", "attack"]:
			if not is_equal_approx(float(a.get(field, 0.0)), float(b.get(field, 0.0))):
				return "enemy " + str(index) + " " + field + " " + str(a.get(field)) + " != " + str(b.get(field))
	return ""

func _test_boss_behaviors(failures: PackedStringArray) -> void:
	var simulation := VerticalSliceSimulation.new()
	var accelerator := simulation._create_enemy("boss_acelerador", 6)
	accelerator.health = accelerator.max_health * 0.3
	_expect_equal("accelerator enrages at low health", simulation._passive_speed_multiplier(accelerator), 2.1, failures)
	var tower := VerticalSliceSimulation.TowerState.new("sentinela", Vector2(5.0, 5.0), simulation.catalog.tower("sentinela"))
	var target := simulation._create_enemy("basico", 1)
	target.position = Vector2(5.0, 5.0)
	var guardian := simulation._create_enemy("boss_guardiao", 6)
	guardian.position = Vector2(6.0, 5.0)
	simulation.enemies = [target, guardian]
	simulation.enemy_spatial.rebuild(simulation.enemies)
	_expect_equal("guardian aura protects nearby enemies", simulation._boss_context_multiplier(tower, target), 0.58, failures)
	var rupture := simulation._create_enemy("boss_ruptura", 8)
	rupture.position = Vector2(5.0, 6.0)
	simulation.enemies = [target, rupture]
	simulation.enemy_spatial.rebuild(simulation.enemies)
	_expect_equal("rupture zone weakens nearby towers", simulation._boss_context_multiplier(tower, target), 0.7, failures)

func _test_account_repository(failures: PackedStringArray) -> void:
	var root := "user://test-accounts-v1"
	var repository := AccountRepository.new(root)
	var catalog := DomainCatalog.new()
	catalog.load_default()
	_cleanup_account_test(root, ["account-1", "account-2", "legacy-1"])
	_expect_equal("first local account created", repository.create_account("account-1", "Tomas"), "", failures)
	_expect_equal("second local account created", repository.create_account("account-2", "Ines"), "", failures)
	_expect_equal("account names are case insensitive", repository.create_account("account-3", "  TOMAS "), "Esse nome ja esta a ser usado", failures)
	var first: Variant = repository.load_account("account-1")
	var second: Variant = repository.load_account("account-2")
	_expect_equal("local accounts remain independent", first is Dictionary and second is Dictionary and first.get("account_id") != second.get("account_id"), true, failures)
	var transfer := {
		"schema": "immutable-towers-transfer",
		"version": 1,
		"source": {"game": "haskell-gloss", "exporter_version": 1},
		"accounts": [{
			"account_id": "legacy-1",
			"name": "Legacy",
			"profile": {"name": "Legacy", "games": 3, "wins": 2, "losses": 1, "best_score": 200},
			"meta_progress": repository.new_account("temp", "Temp").get("meta_progress"),
			"leaderboard": [],
			"selected_mode": "history",
			"pending_run": {
				"base": {"health": 63.0, "credits": 91, "position": {"x": 35.5, "y": 22.5}},
				"portals": [{"position": {"x": 0.5, "y": 2.5}, "waves": [{"cycle": 0.8, "entry_delay": 1.0, "enemies": [{"position": {"x": 0.5, "y": 2.5}, "health": 46.0, "base_speed": 2.5, "speed": 2.5, "attack": 6.0, "loot": 13, "effects": []}]}]}],
				"towers": [{"position": {"x": 8.5, "y": 6.5}, "damage": 15.0, "range": 4.4, "burst": 2, "cycle": 1.3, "remaining_cycle": 0.4, "projectile": {"type_id": "resin", "duration": {"kind": "finite", "seconds": 2.0}}, "runtime": {"tower_id": "sentinela", "level": 2, "specialization": null}}],
				"map_grid": catalog.map_data("planicie_serena").get("grid", []).duplicate(true),
				"active_enemies": [{"position": {"x": 6.5, "y": 2.5}, "health": 30.0, "base_speed": 2.5, "speed": 2.5, "attack": 6.0, "loot": 13, "effects": [{"type_id": "ice", "duration": {"kind": "finite", "seconds": 1.5}}]}],
				"shop": []
			}
		}],
		"warnings": []
	}
	_expect_equal("legacy account import applies", repository.import_transfer(transfer).get("status"), "imported", failures)
	_expect_equal("legacy account import is idempotent", repository.import_transfer(transfer).get("status"), "already_imported", failures)
	var imported_account: Variant = repository.load_account("legacy-1")
	_expect_equal("imported account can be loaded", imported_account is Dictionary, true, failures)
	var converted_run: Variant = imported_account.get("pending_run") if imported_account is Dictionary else null
	_expect_equal("Haskell run converts to Godot schema", converted_run is Dictionary and converted_run.get("schema") == "immutable-towers-run", true, failures)
	_expect_equal("Haskell run preserves base economy", converted_run.get("base_health") == 63.0 and converted_run.get("credits") == 91, true, failures)
	_expect_equal("Haskell runtime tower identity survives", converted_run.get("towers", [])[0].get("tower_id") == "sentinela" and converted_run.get("towers", [])[0].get("level") == 2, true, failures)
	_expect_equal("Haskell enemy class inference matches", converted_run.get("enemies", [])[0].get("class_id") == "rapido", true, failures)
	var converted_simulation := VerticalSliceSimulation.new(catalog, "planicie_serena", "history", PackedStringArray(["sentinela"]))
	_expect_equal("converted Haskell run restores", converted_simulation.restore(converted_run).is_empty() and converted_simulation.wave_definitions.size() == 1, true, failures)
	var legacy_global := transfer.duplicate(true)
	legacy_global["accounts"] = []
	legacy_global["legacy_global"] = {"meta": {
		"profile": {"name": "Global", "games": 1, "wins": 1, "losses": 0, "best_score": 80},
		"meta_progress": repository.new_account("temp", "Temp").get("meta_progress"),
		"leaderboard": [],
		"selected_mode": "history"
	}, "pending_run": null}
	_expect_equal("legacy global save converts to account", repository.import_transfer(legacy_global).get("status"), "imported", failures)
	_expect_equal("converted global account loads", repository.load_account("legacy-global") is Dictionary, true, failures)
	var export_path := root + "/account-export.json"
	_expect_equal("account export writes package", repository.export_account("account-1", export_path), "", failures)
	_expect_equal("account export validates", AccountRepository.AtomicJsonStore.read(export_path).get("schema"), "immutable-towers-account-export", failures)
	_expect_equal("account deletion is isolated", repository.delete_account("account-2").is_empty() and repository.load_account("account-2") == null and repository.load_account("account-1") is Dictionary, true, failures)
	for suffix in ["", ".tmp", ".bak"]:
		var export_file: String = export_path + str(suffix)
		if FileAccess.file_exists(export_file):
			DirAccess.remove_absolute(export_file)
	_cleanup_account_test(root, ["account-1", "account-2", "account-3", "legacy-1", "legacy-global"])

func _test_native_account_round_trip(failures: PackedStringArray) -> void:
	var source_root := "user://test-native-export-source"
	var target_root := "user://test-native-export-target"
	var source := AccountRepository.new(source_root)
	var target := AccountRepository.new(target_root)
	_cleanup_account_test(source_root, ["source-account"])
	_cleanup_account_test(target_root, ["source-account"])
	source.create_account("source-account", "Origem")
	var source_account: Dictionary = source.load_account("source-account")
	source_account["meta_progress"]["gems"] = 321
	source_account["meta_progress"]["unlocked_tower_ids"] = ["sentinela", "glaciar", "tesla"]
	source_account["shop_state"] = source_account["meta_progress"].duplicate(true)
	source_account["leaderboard"] = [{"name": "Origem", "mode_id": "history", "score": 987, "waves": 10}]
	source_account["pending_reward"] = {"tower_id": "tesla", "gems": 0}
	source_account["settings"] = {"fullscreen": true, "reduced_effects": true, "damage_numbers": false}
	var catalog := DomainCatalog.new()
	catalog.load_default()
	var pending_simulation := VerticalSliceSimulation.new(catalog, "planicie_serena", "history", PackedStringArray(["sentinela", "glaciar", "tesla"]))
	pending_simulation.credits = 222
	source_account["pending_run"] = pending_simulation.snapshot()
	source.save_account(source_account)
	var export_path := source_root + "/native-account-export.json"
	_expect_equal("native account export writes backup", source.export_account("source-account", export_path), "", failures)
	var document: Dictionary = AccountRepository.AtomicJsonStore.read(export_path)
	var imported: Dictionary = target.import_document(document)
	_expect_equal("native account imports into clean storage", imported.get("status"), "imported", failures)
	var restored: Dictionary = target.load_account("source-account")
	var exported_account: Dictionary = document.get("exported_account", {})
	_expect_equal(
		"native round trip preserves complete progress",
		restored.get("meta_progress") == exported_account.get("meta_progress")
			and restored.get("leaderboard") == exported_account.get("leaderboard")
			and restored.get("pending_reward") == exported_account.get("pending_reward")
			and restored.get("pending_run") == exported_account.get("pending_run")
			and restored.get("settings") == exported_account.get("settings"),
		true,
		failures
	)
	var changed_document := document.duplicate(true)
	changed_document["exported_account"]["profile"]["games"] = 77
	var conflict := target.import_document(changed_document)
	_expect_equal("native account collision requires a decision", conflict.get("status"), "confirmation_required", failures)
	_expect_equal("cancelled collision keeps local progress", target.load_account("source-account").get("profile", {}).get("games"), 0, failures)
	var copied := target.import_document(changed_document, "import_as_new")
	_expect_equal("native collision can import as new", copied.get("status"), "imported", failures)
	var copied_id := str(copied.get("account_id", ""))
	_expect_equal("native copy receives a new id", not copied_id.is_empty() and copied_id != "source-account", true, failures)
	_expect_equal("native copy preserves imported data", target.load_account(copied_id).get("profile", {}).get("games"), 77, failures)
	var replaced := target.import_document(changed_document, "replace")
	_expect_equal("native collision can replace explicitly", replaced.get("status"), "imported", failures)
	_expect_equal("explicit replacement updates target", target.load_account("source-account").get("profile", {}).get("games"), 77, failures)
	var generated_a := target.generate_account_id()
	var generated_b := target.generate_account_id()
	_expect_equal("generated account ids are robust and distinct", generated_a.begins_with("account-") and generated_b.begins_with("account-") and generated_a != generated_b, true, failures)
	_expect_equal("generated account ids pass repository validation", target.create_account(generated_a, "Gerada A").is_empty() and target.create_account(generated_b, "Gerada B").is_empty(), true, failures)
	target.set_remember_account(false, "source-account")
	target.select_account("source-account")
	_expect_equal("disabled remember account keeps no last session", target.load_index().get("last_account_id"), null, failures)
	target.set_remember_account(true, "source-account")
	_expect_equal("enabled remember account stores last session", target.load_index().get("last_account_id"), "source-account", failures)
	var replacement_account: Dictionary = target.load_account("source-account")
	replacement_account["profile"]["games"] = 88
	target.save_account(replacement_account)
	var primary_path := target_root + "/source-account/profile-v1.json"
	var corrupt_file := FileAccess.open(primary_path, FileAccess.WRITE)
	corrupt_file.store_string("{corrupt")
	corrupt_file.close()
	_expect_equal("native account recovers from atomic backup", target.load_account("source-account") is Dictionary, true, failures)
	for suffix in ["", ".tmp", ".bak"]:
		var export_file := export_path + str(suffix)
		if FileAccess.file_exists(export_file):
			DirAccess.remove_absolute(export_file)
	_cleanup_account_test(source_root, ["source-account"])
	_cleanup_account_test(target_root, ["source-account", copied_id, generated_a, generated_b])

func _test_save_coordinator(failures: PackedStringArray) -> void:
	var coordinator := SaveCoordinator.new(2.0)
	var state := {"saves": 0}
	var saver := func() -> String:
		state["saves"] = int(state.get("saves", 0)) + 1
		return ""
	coordinator.mark_dirty()
	coordinator.tick(1.0, saver)
	_expect_equal("save coordinator does not write before interval", state.get("saves"), 0, failures)
	coordinator.tick(1.1, saver)
	_expect_equal("save coordinator writes after interval", state.get("saves"), 1, failures)
	coordinator.mark_dirty()
	coordinator.flush(saver)
	_expect_equal("save coordinator flushes mutations immediately", state.get("saves") == 2 and not coordinator.dirty, true, failures)

func _test_bot_checkpoint(failures: PackedStringArray) -> void:
	var root := "user://test-bot-checkpoint"
	var repository := AccountRepository.new(root)
	_cleanup_account_test(root, ["bot-save"])
	repository.create_account("bot-save", "Bot Save")
	var app := AppStateClass.new()
	app.repository = repository
	app.catalog = DomainCatalog.new()
	app.catalog.load_default()
	app.active_account = repository.load_account("bot-save")
	var simulation := VerticalSliceSimulation.new(app.catalog, "planicie_serena", "sandbox", PackedStringArray(["sentinela"]))
	var decision := VerticalSliceBot.decide(simulation)
	var outcome := BotRuntime.execute(simulation, decision, func() -> String: return app.save_pending_run(simulation.snapshot()))
	var restored: Dictionary = repository.load_account("bot-save").get("pending_run", {})
	_expect_equal("successful bot mutation checkpoints immediately", outcome.get("status") == "saved" and restored.get("towers", []).size() == simulation.towers.size() and simulation.towers.size() > 0, true, failures)
	var save_outcome := BotRuntime.execute(simulation, {"kind": "save", "reason": "Poupar para Tesla"}, func() -> String: return "checkpoint inesperado")
	_expect_equal("bot saving is a valid explained decision", save_outcome.get("status") == "planned" and save_outcome.get("error", "").is_empty() and not save_outcome.get("mutated", true), true, failures)
	_cleanup_account_test(root, ["bot-save"])
	app.free()

func _cleanup_account_test(root: String, account_ids: Array[String]) -> void:
	for account_id in account_ids:
		for suffix in ["", ".tmp", ".bak"]:
			var profile_path: String = root + "/" + account_id + "/profile-v1.json" + str(suffix)
			if FileAccess.file_exists(profile_path):
				DirAccess.remove_absolute(profile_path)
	for suffix in ["", ".tmp", ".bak"]:
		var index_path: String = root + "/accounts-index-v1.json" + str(suffix)
		if FileAccess.file_exists(index_path):
			DirAccess.remove_absolute(index_path)

func _test_meta_shop(failures: PackedStringArray) -> void:
	var repository := AccountRepository.new("user://unused-shop-test")
	var progress: Dictionary = repository.new_account("shop", "Shop").get("meta_progress")
	var purchase := MetaShop.buy_chest("wood", progress)
	_expect_equal("wood chest uses Haskell deterministic seed", purchase.get("reward", {}).get("tower_id"), "braseiro", failures)
	_expect_equal("new tower chest charges exact gems", purchase.get("progress", {}).get("gems"), 5, failures)
	var duplicate_progress := progress.duplicate(true)
	duplicate_progress["gems"] = 100
	duplicate_progress["unlocked_tower_ids"] = ["sentinela", "glaciar", "braseiro"]
	var duplicate := MetaShop.buy_chest("wood", duplicate_progress)
	_expect_equal("duplicate chest refunds quarter with floor", duplicate.get("progress", {}).get("gems"), 73, failures)
	var fusion_progress := progress.duplicate(true)
	fusion_progress["gems"] = 200
	fusion_progress["unlocked_tower_ids"] = ["sentinela", "tesla", "solar"]
	var fusion := MetaShop.fuse_tempestade(fusion_progress)
	_expect_equal("tempestade fusion unlocks mythical tower", "tempestade" in fusion.get("progress", {}).get("unlocked_tower_ids", []), true, failures)
	_expect_equal("tempestade fusion costs 180 gems", fusion.get("progress", {}).get("gems"), 20, failures)

func _test_shop_presentation(failures: PackedStringArray) -> void:
	var chest_scene := load("res://scenes/ui/shop_chest_card.tscn") as PackedScene
	var chest: Node = chest_scene.instantiate() if chest_scene != null else null
	_expect_equal("shop chest model scene instantiates", chest != null, true, failures)
	if chest != null:
		_expect_equal("shop chest exposes economy metadata", chest.get("chest_id") == "wood" and chest.get("cost") == 35, true, failures)
		chest.call("set_shop_state", false, false)
		_expect_equal("shop chest reflects affordability", chest.get("disabled"), true, failures)
		chest.free()
	var reveal_scene := load("res://scenes/ui/shop_reward_reveal.tscn") as PackedScene
	var reveal: Node = reveal_scene.instantiate() if reveal_scene != null else null
	_expect_equal("shop reward reveal scene instantiates", reveal != null, true, failures)
	if reveal != null:
		reveal.call("present", {"kind": "new_tower", "tower_id": "solar", "gems": 0}, false)
		_expect_equal("shop reward reveal stores pending visual", reveal.visible and reveal.get("reward").get("tower_id") == "solar", true, failures)
		reveal.call("dismiss")
		_expect_equal("shop reward reveal can be dismissed", not reveal.visible, true, failures)
		reveal.free()
	var boot_scene := load("res://scenes/boot/boot.tscn") as PackedScene
	var boot_instance: Node = boot_scene.instantiate() if boot_scene != null else null
	var chest_ids := PackedStringArray()
	if boot_instance != null:
		for node_name in ["WoodChest", "CrystalChest", "ImperialChest"]:
			var card := boot_instance.get_node_or_null("%" + node_name)
			if card != null:
				chest_ids.append(str(card.get("chest_id")))
	_expect_equal("boot shop has three distinct chest models", chest_ids == PackedStringArray(["wood", "crystal", "imperial"]), true, failures)
	if boot_instance != null:
		boot_instance.free()

func _test_map_editor(failures: PackedStringArray) -> void:
	var grid := [
		["grass", "grass", "grass"],
		["path", "asphalt", "path"],
		["grass", "grass", "grass"]
	]
	var editor := MapEditorState.new(grid, Vector2i(0, 1), Vector2i(2, 1))
	_expect_equal("map editor rejects blocked route", editor.cycle_cell(Vector2i(1, 1)).is_empty(), false, failures)
	_expect_equal("map editor accepts safe edit", editor.cycle_cell(Vector2i(1, 0)), "", failures)
	var restored := MapEditorState.new(grid, Vector2i(0, 1), Vector2i(2, 1))
	_expect_equal("custom map snapshot restores", restored.restore(editor.snapshot()), "", failures)
	var catalog := DomainCatalog.new()
	catalog.load_default()
	var old_map: Dictionary = catalog.map_data("planicie_serena")
	var old_custom := {
		"schema": "immutable-towers-custom-map",
		"version": 1,
		"grid": old_map.get("grid", []).duplicate(true),
		"portal": {"x": 0, "y": 2},
		"base": {"x": 35, "y": 22}
	}
	_expect_equal("old custom map migrates to the global grid", restored.restore(old_custom), "", failures)
	_expect_equal("migrated custom map is 12x11", restored.grid.size() == 11 and restored.grid[0].size() == 12, true, failures)
	var custom_simulation := VerticalSliceSimulation.new(null, "planicie_serena", "history", PackedStringArray(["sentinela"]), editor.snapshot())
	_expect_equal("custom map starts as playable simulation", custom_simulation.map_id == "custom" and not custom_simulation.path.is_empty(), true, failures)
	var custom_snapshot := custom_simulation.snapshot()
	var resumed_custom := VerticalSliceSimulation.new()
	_expect_equal("custom map persists in run snapshot", resumed_custom.restore(custom_snapshot).is_empty() and resumed_custom.map_id == "custom", true, failures)
	_expect_equal("custom map grid round trips", resumed_custom.map_grid, custom_simulation.map_grid, failures)
	var editor_scene := load("res://scenes/editor/map_editor.tscn") as PackedScene
	var editor_instance: Node = editor_scene.instantiate() if editor_scene != null else null
	_expect_equal("map editor scene instantiates", editor_instance != null, true, failures)
	if editor_instance != null:
		editor_instance.free()

func _test_bot_haskell_references(failures: PackedStringArray) -> void:
	var catalog := DomainCatalog.new()
	catalog.load_default()
	var references: Array = catalog.document.get("bot_references", [])
	_expect_equal("five Haskell bot references exported", references.size(), 5, failures)
	var fixtures_readable := true
	var decisions_deterministic := true
	var decisions_legal := true
	for reference: Dictionary in references:
		var simulation := VerticalSliceSimulation.new(catalog, str(reference.get("map_id")), str(reference.get("mode_id")), PackedStringArray(["sentinela"]))
		var decision: Dictionary = VerticalSliceBot.decide(simulation)
		var repeated: Dictionary = VerticalSliceBot.decide(simulation)
		var expected: Dictionary = reference.get("action", {})
		var cell: Dictionary = expected.get("cell", {})
		fixtures_readable = fixtures_readable and expected.get("kind") == "build" and expected.get("tower_id") == "sentinela" and int(cell.get("x", -1)) >= 0 and reference.has("score")
		decisions_deterministic = decisions_deterministic and decision.get("kind") == repeated.get("kind") and decision.get("cell") == repeated.get("cell") and decision.get("tower_id") == repeated.get("tower_id")
		decisions_legal = decisions_legal and decision.get("kind") == "build" and decision.get("tower_id") == "sentinela" and simulation.is_buildable(decision.get("cell", Vector2i(-1, -1)))
	_expect_equal("historical Haskell bot fixtures remain readable", fixtures_readable, true, failures)
	_expect_equal("global-grid bot decisions remain deterministic", decisions_deterministic, true, failures)
	_expect_equal("global-grid bot decisions are legal", decisions_legal, true, failures)

func _test_run_score(failures: PackedStringArray) -> void:
	var simulation := VerticalSliceSimulation.new()
	simulation.base_health = 60.0
	simulation.credits = 120
	simulation.enemies_defeated = 45
	simulation.elapsed = 300.0
	var quick_score := RunScore.calculate(simulation, 8)
	simulation.elapsed = 600.0
	var slow_score := RunScore.calculate(simulation, 8)
	_expect_equal("waiting never increases score", slow_score <= quick_score, true, failures)
	var before_spam := RunScore.calculate(simulation, 8)
	simulation.towers.append(VerticalSliceSimulation.TowerState.new("sentinela", Vector2(8.5, 6.5), simulation.catalog.tower("sentinela")))
	_expect_equal("adding an unused tower never increases score", RunScore.calculate(simulation, 8), before_spam, failures)
	var deterministic_score := RunScore.calculate(simulation, 8)
	_expect_equal("run score is deterministic", RunScore.calculate(simulation, 8), deterministic_score, failures)
	var snapshot := simulation.snapshot()
	var restored := VerticalSliceSimulation.new()
	_expect_equal("score inputs survive save and load", restored.restore(snapshot).is_empty() and RunScore.calculate(restored, 8) == deterministic_score, true, failures)

func _test_run_rewards(failures: PackedStringArray) -> void:
	var expected_rewards := {
		0: 0,
		1: 3,
		5: 13,
		10: 30,
		15: 50,
		20: 70,
		30: 95,
		50: 125,
		100: 173,
		120: 188
	}
	for wave: int in expected_rewards:
		_expect_equal("infinite reward at wave " + str(wave), RunRewards.infinite_gems(wave), expected_rewards[wave], failures)
	var previous_reward := 0
	var monotonic_and_capped := true
	for wave in range(0, 301):
		var reward := RunRewards.infinite_gems(wave)
		monotonic_and_capped = monotonic_and_capped and reward >= previous_reward and reward <= RunRewards.INFINITE_REWARD_CAP
		previous_reward = reward
	_expect_equal("infinite rewards are monotonic and capped", monotonic_and_capped, true, failures)

func _test_app_progression(failures: PackedStringArray) -> void:
	var root := "user://test-app-state-v1"
	var repository := AccountRepository.new(root)
	_cleanup_account_test(root, ["progress"])
	repository.create_account("progress", "Progress")
	var app := AppStateClass.new()
	app.repository = repository
	app.catalog = DomainCatalog.new()
	app.catalog.load_default()
	app.active_account = repository.load_account("progress")
	app.selected_map_id = "planicie_serena"
	var empty_root := "user://test-empty-app-state"
	var empty_repository := AccountRepository.new(empty_root)
	_cleanup_account_test(empty_root, [])
	var empty_app := AppStateClass.new()
	empty_app.repository = empty_repository
	empty_app.catalog = app.catalog
	empty_app._load_or_create_account()
	_expect_equal("first boot waits for explicit local account", empty_app.active_account.is_empty() and empty_repository.load_index().get("accounts", []).is_empty(), true, failures)
	empty_app.free()
	_expect_equal("account settings have safe defaults", app.settings(), {"fullscreen": false, "reduced_effects": false, "damage_numbers": true}, failures)
	app.set_setting("reduced_effects", true)
	app.set_setting("damage_numbers", false)
	var saved_settings: Dictionary = repository.load_account("progress").get("settings", {})
	_expect_equal("account settings survive repository reload", saved_settings.get("reduced_effects") == true and saved_settings.get("damage_numbers") == false, true, failures)
	_expect_equal("locked mode respects account level", app.mode_unlocked("boss"), false, failures)
	var simulation := VerticalSliceSimulation.new(app.catalog, "planicie_serena", "history", PackedStringArray(["sentinela"]))
	var pending_snapshot := simulation.snapshot()
	_expect_equal("pending run saves in active account", app.save_pending_run(pending_snapshot), "", failures)
	_expect_equal("pending run is detected", app.has_pending_run(), true, failures)
	_expect_equal("pending run survives repository reload", repository.load_account("progress").get("pending_run", {}).get("schema"), "immutable-towers-run", failures)
	_expect_equal("pending run can be cleared", app.clear_pending_run(), "", failures)
	simulation.finished = true
	simulation.victory = true
	simulation.wave_index = 9
	_expect_equal("completed run persists", app.complete_run(simulation), "", failures)
	_expect_equal("history victory grants exact gems", app.active_account.get("meta_progress", {}).get("gems"), 80, failures)
	_expect_equal("history victory advances stage", app.active_account.get("meta_progress", {}).get("history_stage"), 2, failures)
	_expect_equal("completed run updates profile and ranking", app.active_account.get("profile", {}).get("wins") == 1 and app.active_account.get("leaderboard", []).size() == 1, true, failures)
	app.active_account["selected_mode"] = "infinite"
	app.active_account["meta_progress"].erase("best_infinite_wave")
	var infinite := VerticalSliceSimulation.new(app.catalog, "planicie_serena", "infinite", PackedStringArray(["sentinela"]))
	infinite.finished = true
	infinite.victory = false
	infinite.wave_index = 19
	_expect_equal("infinite defeat persists", app.complete_run(infinite), "", failures)
	_expect_equal("infinite wave twenty grants balanced gems", app.active_account.get("meta_progress", {}).get("gems"), 150, failures)
	_expect_equal("infinite best wave is persisted for legacy accounts", app.active_account.get("meta_progress", {}).get("best_infinite_wave"), 20, failures)
	_expect_equal("infinite result exposes reward to UI", app.last_run_gem_reward == 70 and app.last_run_wave_reached == 20, true, failures)
	_expect_equal(
		"infinite leaderboard records reached wave",
		app.active_account.get("leaderboard", []).any(
			func(entry: Dictionary) -> bool: return entry.get("mode_id") == "infinite" and entry.get("waves") == 20
		),
		true,
		failures
	)
	_cleanup_account_test(root, ["progress"])
	app.free()
