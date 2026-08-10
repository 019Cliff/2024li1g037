class_name MapEditorState
extends RefCounted

const MapPathfinder = preload("res://src/gameplay/map_pathfinder.gd")
const WorldGrid = preload("res://src/domain/world_grid.gd")

const TERRAIN_CYCLE := {"grass": "path", "path": "asphalt", "asphalt": "water", "water": "grass"}

var grid: Array = []
var portal := Vector2i.ZERO
var base := Vector2i.ZERO

func _init(source_grid: Array, portal_cell: Vector2i, base_cell: Vector2i) -> void:
	grid = source_grid.duplicate(true)
	portal = portal_cell
	base = base_cell

func cycle_cell(cell: Vector2i) -> String:
	if not _inside(cell):
		return "Celula fora do mapa"
	var candidate: Array = grid.duplicate(true)
	candidate[cell.y][cell.x] = TERRAIN_CYCLE.get(str(candidate[cell.y][cell.x]), "grass")
	var validation := validate(candidate, portal, base)
	if not validation.is_empty():
		return validation
	grid = candidate
	return ""

func set_portal(cell: Vector2i) -> String:
	var validation := validate(grid, cell, base)
	if validation.is_empty():
		portal = cell
	return validation

func set_base(cell: Vector2i) -> String:
	var validation := validate(grid, portal, cell)
	if validation.is_empty():
		base = cell
	return validation

func validate(candidate_grid: Array = grid, candidate_portal: Vector2i = portal, candidate_base: Vector2i = base) -> String:
	if not MapPathfinder._is_route_cell(candidate_grid, candidate_base):
		return "A base tem de permanecer no caminho"
	if not MapPathfinder._is_route_cell(candidate_grid, candidate_portal):
		return "O portal tem de permanecer no caminho"
	if MapPathfinder.find_path(candidate_grid, candidate_portal, candidate_base).is_empty():
		return "A edicao bloqueava o caminho ate a base"
	return ""

func snapshot() -> Dictionary:
	return {
		"schema": "immutable-towers-custom-map",
		"version": 2,
		"world_grid_scale": WorldGrid.SOURCE_CELLS_PER_CELL,
		"grid": grid.duplicate(true),
		"portal": {"x": portal.x, "y": portal.y},
		"base": {"x": base.x, "y": base.y}
	}

func restore(document: Variant) -> String:
	if not document is Dictionary or document.get("schema") != "immutable-towers-custom-map" or int(document.get("version", 0)) not in [1, 2]:
		return "Mapa guardado invalido"
	var normalized := WorldGrid.normalize_custom_document(document)
	if not normalized.get("grid") is Array or not normalized.get("portal") is Dictionary or not normalized.get("base") is Dictionary:
		return "Mapa guardado incompleto"
	var portal_data: Dictionary = normalized.get("portal")
	var base_data: Dictionary = normalized.get("base")
	var candidate_portal := Vector2i(int(portal_data.get("x", -1)), int(portal_data.get("y", -1)))
	var candidate_base := Vector2i(int(base_data.get("x", -1)), int(base_data.get("y", -1)))
	var candidate_grid: Array = normalized.get("grid").duplicate(true)
	var validation := validate(candidate_grid, candidate_portal, candidate_base)
	if not validation.is_empty():
		return validation
	grid = candidate_grid
	portal = candidate_portal
	base = candidate_base
	return ""

func _inside(cell: Vector2i) -> bool:
	return cell.y >= 0 and cell.y < grid.size() and grid[cell.y] is Array and cell.x >= 0 and cell.x < grid[cell.y].size()
