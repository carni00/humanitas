#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/custom.h>

#include <SDL/SDL.h>

#include <SDL/SDL_gfxPrimitives.h>



typedef void (*sdl_finalizer)(void *);

struct ml_sdl_surf_data {
  SDL_Surface *s ;
  int freeable;
  sdl_finalizer finalizer;
  void *finalizer_data;
};


static inline SDL_Surface *SDL_SURFACE(value v)
{
  struct ml_sdl_surf_data *cb_data;
  cb_data = (Tag_val(v) == 0) ? 
    Data_custom_val(Field(v, 0)) : Data_custom_val(v);
  return cb_data->s;
}



CAMLprim value ml_SDL_line_native(value surf, 
				      value x1, value y1, 
				      value x2, value y2, 
				      value pix)
{
  SDL_Surface *s = SDL_SURFACE(surf);
  Uint32 pixel = Int32_val(pix);
  lineColor(s, Int_val(x1), Int_val(y1), Int_val(x2), Int_val(y2), pixel);
  return Val_unit;
}

CAMLprim value ml_SDL_line_bytecode(value * argv, int argn)
{
  return ml_SDL_line_native(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5]);
}

CAMLprim value ml_SDL_triangle_native(value surf, 
				      value x1, value y1, 
				      value x2, value y2,
				      value x3, value y3, value pix)
{
  SDL_Surface *s = SDL_SURFACE(surf);
  Uint32 pixel = Int32_val(pix);
  trigonColor(s, Int_val(x1), Int_val(y1), Int_val(x2), Int_val(y2), Int_val(x3), Int_val(y3), pixel);
  return Val_unit;
}

CAMLprim value ml_SDL_triangle_bytecode(value * argv, int argn)
{
  return ml_SDL_triangle_native(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6],argv[7]);
}

int gfxPrimitivesCompareInt(const void *a, const void *b)
{
    return (*(const int *) a) - (*(const int *) b);
}

static int *gfxPrimitivesPolyIntsGlobal = NULL;
static int gfxPrimitivesPolyAllocatedGlobal = 0;

Uint32 modCol(Uint32 c, int e) {
  return c + e;
}


int text_filledPolygonColor(SDL_Surface * dst, const Sint16 * vx, const Sint16 * vy, int n, Uint32 color, unsigned int seed, int gran)
{
    int result;
    int i,j;
    int y, xa, xb;
    int miny, maxy;
    int x1, y1;
    int x2, y2;
    int ind1, ind2;
    int ints;
    int *gfxPrimitivesPolyInts = NULL;
    int gfxPrimitivesPolyAllocated = 0;

    int **polyInts = NULL;
    int *polyAllocated = NULL;

    /*
     * Check visibility of clipping rectangle
     */
    if ((dst->clip_rect.w==0) || (dst->clip_rect.h==0)) {
     return(0);
    }

    /*
     * Sanity check number of edges
     */
    if (n < 3) {
	return -1;
    }
     
    /*
     * Map polygon cache  
     */
    if ((polyInts==NULL) || (polyAllocated==NULL)) {
       /* Use global cache */
       gfxPrimitivesPolyInts = gfxPrimitivesPolyIntsGlobal;
       gfxPrimitivesPolyAllocated = gfxPrimitivesPolyAllocatedGlobal;
    } else {
       /* Use local cache */
       gfxPrimitivesPolyInts = *polyInts;
       gfxPrimitivesPolyAllocated = *polyAllocated;
    }

    /*
     * Allocate temp array, only grow array 
     */
    if (!gfxPrimitivesPolyAllocated) {
	gfxPrimitivesPolyInts = (int *) malloc(sizeof(int) * n);
	gfxPrimitivesPolyAllocated = n;
    } else {
	if (gfxPrimitivesPolyAllocated < n) {
	    gfxPrimitivesPolyInts = (int *) realloc(gfxPrimitivesPolyInts, sizeof(int) * n);
	    gfxPrimitivesPolyAllocated = n;
	}
    }

    /*
     * Check temp array
     */
    if (gfxPrimitivesPolyInts==NULL) {        
      gfxPrimitivesPolyAllocated = 0;
    }

    /*
     * Update cache variables
     */
    if ((polyInts==NULL) || (polyAllocated==NULL)) { 
     gfxPrimitivesPolyIntsGlobal =  gfxPrimitivesPolyInts;
     gfxPrimitivesPolyAllocatedGlobal = gfxPrimitivesPolyAllocated;
    } else {
     *polyInts = gfxPrimitivesPolyInts;
     *polyAllocated = gfxPrimitivesPolyAllocated;
    }

    /*
     * Check temp array again
     */
    if (gfxPrimitivesPolyInts==NULL) {        
	return(-1);
    }

    /*
     * Determine Y maxima 
     */
    miny = vy[0];
    maxy = vy[0];
    for (i = 1; (i < n); i++) {
	if (vy[i] < miny) {
	    miny = vy[i];
	} else if (vy[i] > maxy) {
	    maxy = vy[i];
	}
    }

    /* Extracting color components */
    Uint32 r = (color & 0xff000000) >> 24;
    Uint32 g = (color & 0x00ff0000) >> 16;
    Uint32 b = (color & 0x0000ff00) >>  8;
    Uint32 a = (color & 0x000000ff);

    /*
     * Draw, scanning y 
     */
    result = 0;
    for (y = miny; (y <= maxy); y++) {
	ints = 0;
	for (i = 0; (i < n); i++) {
	    if (!i) {
		ind1 = n - 1;
		ind2 = 0;
	    } else {
		ind1 = i - 1;
		ind2 = i;
	    }
	    y1 = vy[ind1];
	    y2 = vy[ind2];
	    if (y1 < y2) {
		x1 = vx[ind1];
		x2 = vx[ind2];
	    } else if (y1 > y2) {
		y2 = vy[ind1];
		y1 = vy[ind2];
		x2 = vx[ind1];
		x1 = vx[ind2];
	    } else {
		continue;
	    }
	    if ( ((y >= y1) && (y < y2)) || ((y == maxy) && (y > y1) && (y <= y2)) ) {
		gfxPrimitivesPolyInts[ints++] = ((65536 * (y - y1)) / (y2 - y1)) * (x2 - x1) + (65536 * x1);
	    } 	    
	}
	
	qsort(gfxPrimitivesPolyInts, ints, sizeof(int), gfxPrimitivesCompareInt);

	for (i = 0; (i < ints); i += 2) {
	    xa = gfxPrimitivesPolyInts[i] + 1;
	    xa = (xa >> 16) + ((xa & 32768) >> 15);
	    xb = gfxPrimitivesPolyInts[i+1] - 1;
	    xb = (xb >> 16) + ((xb & 32768) >> 15);
	    for(j = xa ; j <= xb; j++) {
	      int rr = rand_r(&seed);
	      unsigned int sg = seed+1;
	      int rg = rand_r(&sg);//abs(rr*rr);
	      unsigned int sb = seed+2;
	      int rb = rand_r(&sb);//abs(rg*rg);
	      int mg = (gran/2)>2?(gran/2):2;
	      Uint32 dr = (rr%mg==0?1:(-1))*(rr%gran);
	      Uint32 dg = (rg%mg==0?1:(-1))*(rg%gran);
	      Uint32 db = (rb%mg==0?1:(-1))*(rb%gran);

	      Uint32 c = ((r+dr) << 24) | ((g+dg) << 16) | ((b+db) << 8) | a;
	      result |= hlineColor(dst, j, j, y, c);
	    }
	}
    }

    return (result);
}


int text_filledTrigonColor(SDL_Surface * dst, Sint16 x1, Sint16 y1, Sint16 x2, Sint16 y2, Sint16 x3, Sint16 y3, Uint32 color, unsigned int seed, int gran)
{
 Sint16 vx[3]; 
 Sint16 vy[3];
 
 vx[0]=x1;
 vx[1]=x2;
 vx[2]=x3;
 vy[0]=y1;
 vy[1]=y2;
 vy[2]=y3;
 
 return(text_filledPolygonColor(dst,vx,vy,3,color,seed,gran));
}


CAMLprim value ml_SDL_filled_triangle_native(value surf, 
					     value x1, value y1, 
					     value x2, value y2,
					     value x3, value y3, value pix)
{
  SDL_Surface *s = SDL_SURFACE(surf);
  Uint32 pixel = Int32_val(pix);
  filledTrigonColor(s, Int_val(x1), Int_val(y1), Int_val(x2), Int_val(y2), Int_val(x3), Int_val(y3), pixel);
  return Val_unit;
}

CAMLprim value ml_SDL_filled_triangle_bytecode(value * argv, int argn)
{
  return ml_SDL_filled_triangle_native(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6],argv[7]);
}


CAMLprim value ml_text_filled_triangle_native(value surf, 
					     value x1, value y1, 
					     value x2, value y2,
					     value x3, value y3, 
					     value pix, value caml_seed, value caml_gran)
{
  SDL_Surface *s = SDL_SURFACE(surf);
  Uint32 pixel = Int32_val(pix);
  int seed = Int_val(caml_seed);
  int gran = Int_val(caml_gran);
  text_filledTrigonColor(s, Int_val(x1), Int_val(y1), Int_val(x2), Int_val(y2), Int_val(x3), Int_val(y3), pixel, seed, gran);
  return Val_unit;
}

CAMLprim value ml_text_filled_triangle_bytecode(value * argv, int argn)
{
  return ml_text_filled_triangle_native(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5],argv[6],argv[7],argv[8], argv[9] );
}

