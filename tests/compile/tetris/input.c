int printf(char *, ...);

void *malloc(int);
void *calloc(int, int);

struct Tile {
	char *buffer;
	int width, height;
	int rot; // lazy rotation
};

struct Tile *board;

struct Tile *tile_create(int width, int height) {
	struct Tile *tile = malloc(sizeof(struct Tile));
	tile->width = width;
	tile->height = height;
	tile->buffer = calloc(1, sizeof(char) * width * height);
	tile->rot = 0;
	return tile;
}

void tile_size(struct Tile *tile, int *w, int *h) {
	*w = (tile->rot % 2) ? tile->height : tile->width;
	*h = (tile->rot % 2) ? tile->width : tile->height;
}

char *tile_get(struct Tile *tile, int x, int y) {
	int w = tile->width, h = tile->height;

	if (tile->rot == 2) {
		x = (w - 1) - x;
		y = (h - 1) - y;
	} else {
		int r = tile->rot;
		while (r --> 0) {
			int tmp;

			tmp = y;
			y = (h - 1) - x;
			x = tmp;

			tmp = w;
			w = h;
			h = tmp;
		}
	}

	return &tile->buffer[x + y * tile->width];
}

void tile_rotate(struct Tile *tile) {
	tile->rot = (tile->rot + 1) % 4;
}

void tile_print(struct Tile *tile) {
	int w, h;
	tile_size(tile, &w, &h);

	int y = 0;
	while (y < h) {
		int x = 0;
		while (x < w) {
			char v = *tile_get(tile, x, y);
			v = v ? (v + '0') : ' ';
			printf("%c%c", v, v);
			++x;
		}
		++y;
		printf("\n");
	}
	printf("\n");
}

void place(struct Tile *piece, int x, int y) {
	int tw, th;
	tile_size(piece, &tw, &th);

	int ty = 0;
	while (ty < th) {
		int tx = 0;
		while (tx < tw) {
			char v = *tile_get(piece, tx, ty);
			if (v)
				*tile_get(board, tx+x, ty+y) = v;
			++tx;
		}
		++ty;
	}
}

struct Tile *helper(char *str, int w, int h, int num) {
	struct Tile *t = tile_create(w, h);
	int y = 0;
	while (y < h) {
		int x = 0;
		while (x < w) {
			char v = str[x + y * w];
			*tile_get(t, x, y) = v == ' ' ? 0 : num;
			++x;
		}
		++y;
	}

	return t;
}

int tile_height(struct Tile *t, int x, int yd) {
	int tw, th;
	tile_size(t, &tw, &th);

	int y = yd < 0 ? th - 1 : 0;
	while ((y >= 0-yd && y <= th-yd) && !*tile_get(t, x, y))
		y = y + yd;
	return y;
}

int heuristic(struct Tile *t, int x, int *y) {
	int tw, th;
	tile_size(t, &tw, &th);

	int tx = 0;
	*y = board->height;

	int h = 0;

	while (tx < tw) {
		// find board height
		int bh = tile_height(board, x+tx, +1);
		int th = tile_height(t, tx, -1);

		int ly = bh - th - 1;
		int hd = 0;
		if (*y < board->height) {
			int d = *y - ly;
			h = h + 4 * d * d;
		}

		if (ly < *y)
			*y = ly;

		++tx;
	}

	h = h + board->height - *y;

	return h;
}

void place_best(struct Tile *t) {
	struct {
		int rot;
		int x, y;
		int h;
	} best;

	int inf = 1000000;
	best.h = inf;

	t->rot = 0;
	while (t->rot < 4) {
		int tw, th;
		tile_size(t, &tw, &th);

		int x = 0;
		while (x <= board->width - tw) {
			int y;
			int h = heuristic(t, x, &y);
			//printf("%d, %d: %d\n", x, y, h);

			if (h < best.h) {
				best.h = h;
				best.rot = t->rot;
				best.x = x;
				best.y = y;
			}

			++x;
		}

		++t->rot;
	}

	if (best.h == inf)
		return; // shouldn't happen!

	t->rot = best.rot;
	place(t, best.x, best.y);
}

struct Tile *tetris_T(int n) {
	return helper("xxx x ", 3, 2, n);
}

struct Tile *tetris_O(int n) {
	return helper("xxxx", 2, 2, n);
}

struct Tile *tetris_L(int n) {
	return helper("x x xx", 2, 3, n);
}

struct Tile *tetris_Z(int n) {
	return helper(" xxxx ", 3, 2, n);
}

struct Tile *tetris_I(int n) {
	return helper("xxxx", 1, 4, n);
}

int main(void) {
	int pcsCount = 5;

	struct Tile *(**pcs)(int) = malloc(sizeof(void *) * pcsCount);
	pcs[0] = tetris_T;
	pcs[1] = tetris_O;
	pcs[2] = tetris_L;
	pcs[3] = tetris_Z;
	pcs[4] = tetris_I;

	board = tile_create(10, 16);

	/*int i = 0;
	while (i < 6) {
		*tile_get(board, i, i+4) = 1;
		++i;
	}*/

	int j = 0;
	while (j < 30) {
		struct Tile *t = pcs[j % pcsCount]((j % 9) + 1);
		place_best(t);
		//tile_print(board);
		++j;
	}

	tile_print(board);
}
