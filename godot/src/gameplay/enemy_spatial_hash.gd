class_name EnemySpatialHash
extends RefCounted

var cell_size: float
var buckets: Dictionary[Vector2i, Array] = {}

func _init(bucket_size: float = 6.0) -> void:
	cell_size = maxf(1.0, bucket_size)

func rebuild(enemies: Array) -> void:
	for bucket: Array in buckets.values():
		bucket.clear()
	for enemy: Variant in enemies:
		var key := _cell_for(enemy.position)
		if not buckets.has(key):
			buckets[key] = []
		buckets[key].append(enemy)

func query(position: Vector2, radius: float) -> Array:
	var result: Array = []
	var minimum := _cell_for(position - Vector2.ONE * radius)
	var maximum := _cell_for(position + Vector2.ONE * radius)
	for y in range(minimum.y, maximum.y + 1):
		for x in range(minimum.x, maximum.x + 1):
			var key := Vector2i(x, y)
			if buckets.has(key):
				result.append_array(buckets[key])
	return result

func _cell_for(position: Vector2) -> Vector2i:
	return Vector2i(floori(position.x / cell_size), floori(position.y / cell_size))
