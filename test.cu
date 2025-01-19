#include <stddef.h>
#include <stdint.h>

constexpr const int num_buckets = 16;

constexpr const int top_step_x = 32;
cosntexpr const int top_step_y = 4;

__global__ void tree_top_accumulator(const int64_t nrows, const int64_t nfolds,
                                     const int64_t npicks, const float *const g,
                                     const int64_t *const picked_features,
                                     const uint8_t *const x,
                                     const size_t x_pitch,
                                     float *const output) {
  int j = blockIdx.x;
  int f = blockIdx.y;

  int group_idx = threadIdx.x + threadIdx.y * top_step_x;

  const int nrows_over_2 = nrows / 2;

  const uint8_t *this_x = x + x_pitch * picked_features[f];
  const float *g_for_j = g + j * nrows;

  __shared__ float all_temp_output[num_buckets * top_step_x * top_step_y];
  float *this_temp_out = all_temp_output + group_idx;

  for (int i = 0; i < num_buckets; i++) {
    this_temp_out[i * top_step_x * top_step_y] = 0.0f;
  }

  for (int idx = group_idx * 4; idx < nrows_over_2;
       idx += top_step_x * top_step_y * 4) {
    int this_int = *reinterpret_cast<const int *>(this_x + idx);
    float4 g0 = *reinterpret_cast<const float4 *>(g_for_j + idx * 2);
    float4 g1 = *reinterpret_cast<const float4 *>(g_for_j + idx * 2 + 4);

    int val_0_x = (this_int >> 0x00) & 0x0f;
    int val_0_y = (this_int >> 0x04) & 0x0f;
    int val_0_z = (this_int >> 0x08) & 0x0f;
    int val_0_w = (this_int >> 0x0c) & 0x0f;
    int val_1_x = (this_int >> 0x10) & 0x0f;
    int val_1_y = (this_int >> 0x14) & 0x0f;
    int val_1_z = (this_int >> 0x18) & 0x0f;
    int val_1_w = (this_int >> 0x1c) & 0x0f;

    this_temp_out[val_0_x * top_step_x * top_step_y] += g0.x;
    this_temp_out[val_0_y * top_step_x * top_step_y] += g0.y;
    this_temp_out[val_0_z * top_step_x * top_step_y] += g0.z;
    this_temp_out[val_0_w * top_step_x * top_step_y] += g0.w;
    this_temp_out[val_1_x * top_step_x * top_step_y] += g1.x;
    this_temp_out[val_1_y * top_step_x * top_step_y] += g1.y;
    this_temp_out[val_1_z * top_step_x * top_step_y] += g1.z;
    this_temp_out[val_1_w * top_step_x * top_step_y] += g1.w;
  }

  float *output_for_j = output + f * num_buckets + j * npick * num_buckets;

  for (int i = 0; i < num_buckets; i++) {
    atomicAdd(output_for_j + i, this_temp_out[i * top_step_x * top_step_y]);
  }
}
