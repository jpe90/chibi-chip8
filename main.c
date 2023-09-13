#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <chibi/sexp.h>
#include "SDL.h"

#define WINDOW_HEIGHT 640
#define WINDOW_WIDTH 1280
#define SCREEN_HEIGHT 32
#define SCREEN_WIDTH 64

#define NUM_KEYS 16

static void init_window(SDL_Window **window, SDL_Renderer **renderer, SDL_Texture **sdl_texture);
static sexp buffer_graphics(sexp ctx, sexp self, sexp n, sexp bytevector);
static sexp draw_graphics(sexp ctx, sexp self, sexp n);
static void close_window(SDL_Window *window, SDL_Renderer *renderer, SDL_Texture *texture);
static sexp process_user_input(sexp ctx, sexp self, sexp n);

static bool is_running = true;
static bool is_paused = false;

static bool keyboard[NUM_KEYS] = {false};

static SDL_Window *screen;
static SDL_Renderer *renderer;
static SDL_Texture *texture;
static uint32_t pixel_buffer[SCREEN_HEIGHT * SCREEN_WIDTH] = {0};
static SDL_Event e;

const static uint8_t keymap[NUM_KEYS] = {
    SDLK_x,
    SDLK_1,
    SDLK_2,
    SDLK_3,
    SDLK_q,
    SDLK_w,
    SDLK_e,
    SDLK_a,
    SDLK_s,
    SDLK_d,
    SDLK_z,
    SDLK_c,
    SDLK_4,
    SDLK_r,
    SDLK_f,
    SDLK_v 
};

static void init_window(SDL_Window **window, SDL_Renderer **renderer, SDL_Texture **texture)
{
  (*window) = SDL_CreateWindow(
      "CHIP-8",
      SDL_WINDOWPOS_CENTERED,
      SDL_WINDOWPOS_CENTERED,
      WINDOW_WIDTH,
      WINDOW_HEIGHT,
      SDL_WINDOW_SHOWN |
      SDL_WINDOW_OPENGL |
      SDL_WINDOW_RESIZABLE |
      SDL_WINDOW_ALLOW_HIGHDPI
  );

  if ((*window) == NULL)
  {
    printf("Could not create SDL Window: %s\n", SDL_GetError());
    exit(1);
  }

  (*renderer) = SDL_CreateRenderer((*window), -1, 0);

  if (*renderer == NULL)
  {
    printf("Could not create SDL Renderer: %s\n", SDL_GetError());
    exit(1);
  }

  (*texture) = SDL_CreateTexture((*renderer),
                                 SDL_PIXELFORMAT_RGBA8888,
                                 SDL_TEXTUREACCESS_TARGET,
                                 SCREEN_WIDTH,
                                 SCREEN_HEIGHT);

  if (*texture == NULL)
  {
    printf("Could not create SDL Texture: %s\n", SDL_GetError());
    exit(1);
  }

  SDL_SetRenderDrawColor(*renderer, 0, 0, 0, 0);
  SDL_RenderClear(*renderer);
  SDL_RenderPresent(*renderer);
}

static sexp process_user_input(sexp ctx, sexp self, sexp n)
{
  while (SDL_PollEvent(&e))
  {
    if (e.type == SDL_KEYDOWN)
    {
      switch (e.key.keysym.sym)
      {
      case SDLK_ESCAPE:
        exit(0);
        is_running = false;
        break;
      case SDLK_SPACE:
        if (is_paused)
        {
          is_paused = false;
        }
        else
        {
          is_paused = true;
        }
        break;
      default:
        break;
      }
      for (int i = 0; i < NUM_KEYS; i++)
      {
        if (e.key.keysym.sym == keymap[i])
        {
          keyboard[i] = true;
        }
      }
    }
    if (e.type == SDL_KEYUP)
    {
      for (int i = 0; i < NUM_KEYS; i++)
      {
        if (e.key.keysym.sym == keymap[i])
        {
          keyboard[i] = false;
        }
      }
    }
    if (e.type == SDL_QUIT)
    {
      exit(0);
    }
  }
  return SEXP_VOID;
}

static sexp is_pausedp(sexp ctx, sexp self, sexp n)
{
  if (is_paused)
  {
    return SEXP_TRUE;
  }
  else
  {
    return SEXP_FALSE;
  }
}

static sexp is_runningp(sexp ctx, sexp self, sexp n)
{
  if (is_running)
  {
    return SEXP_TRUE;
  }
  else
  {
    return SEXP_FALSE;
  }
}

static sexp key_pressed(sexp ctx, sexp self, sexp n, sexp key)
{
  int key_index = sexp_unbox_fixnum(key);
  if (keyboard[key_index])
  {
    return SEXP_TRUE;
  }
  else
  {
    return SEXP_FALSE;
  }
}

static sexp buffer_graphics(sexp ctx, sexp self, sexp n, sexp bytevector)
{
    uint8_t* data = (uint8_t*)sexp_bytes_data(bytevector);
    int length = sexp_bytes_length(bytevector);
    for (int i = 0; i < length; i++)
    {
        uint8_t pixel = data[i];
        pixel_buffer[i] = (0xFFFFFF00 * pixel) | 0x000000FF;
    }
    return SEXP_VOID;
}

static sexp draw_graphics(sexp ctx, sexp self, sexp n)
{
  SDL_UpdateTexture(texture, NULL, pixel_buffer, SCREEN_WIDTH * sizeof(uint32_t));
  SDL_RenderClear(renderer);
  SDL_RenderCopy(renderer, texture, NULL, NULL);
  SDL_RenderPresent(renderer);
  return SEXP_VOID;
}

static void close_window(SDL_Window *window, SDL_Renderer *renderer, SDL_Texture *texture)
{
  SDL_DestroyWindow(window);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyTexture(texture);
  SDL_Quit();
}

int running = 1;

void provide(sexp ctx, sexp env, char *name, int num_args, void *func)
{
  sexp op = sexp_define_foreign(ctx, env, name, num_args, func);
  if (sexp_opcodep(op))
  {
    sexp_opcode_return_type(op) = SEXP_VOID;
  }
  else
  {
    printf("could not register %s!\n", name);
  }
}

void scheme_define_ops(sexp ctx, sexp env)
{
  provide(ctx, env, "buffer-graphics", 1, (sexp_proc1)buffer_graphics);
  provide(ctx, env, "draw-graphics", 0, (sexp_proc1)draw_graphics);
  provide(ctx, env, "process-user-input", 0, (sexp_proc1)process_user_input);
  provide(ctx, env, "is-paused?", 0, (sexp_proc1)is_pausedp);
  provide(ctx, env, "is-running?", 0, (sexp_proc1)is_runningp);
  provide(ctx, env, "key-pressed?", 1, (sexp_proc1)key_pressed);
}

int main(int argc, char **argv)
{
  sexp ctx;

  SDL_Init(SDL_INIT_EVERYTHING);
  init_window(&screen, &renderer, &texture);
  sexp_scheme_init();

  ctx = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);
  sexp res;
  res = sexp_load_standard_env(ctx, NULL, SEXP_SEVEN);
  if (sexp_exceptionp(res))
  {
    sexp_print_exception(ctx, res, sexp_current_error_port(ctx));
    exit(1);
  }
  sexp_load_standard_ports(ctx, NULL, stdin, stdout, stderr, 1);
  scheme_define_ops(ctx, sexp_context_env(ctx));
  sexp_gc_var1(obj1);
  sexp_gc_preserve1(ctx, obj1);

  obj1 = sexp_c_string(ctx, "./chip8.scm", -1);

  res = sexp_load(ctx, obj1, NULL);
  if (sexp_exceptionp(res))
  {
    sexp_print_exception(ctx, res, sexp_current_error_port(ctx));
    exit(1);
  }

  sexp_gc_release1(ctx);
  sexp_destroy_context(ctx);
  close_window(screen, renderer, texture);
}