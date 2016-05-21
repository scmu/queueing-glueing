#include <stdio.h>
const int N = 1000005;
typedef long long i64;

i64 x[N], s[N], dp[N];
i64 L = 71;
int que[N], pi[N], parts[N];

inline i64 sq(const i64 &x) { return x * x; }

double delta(int j, int k) { return (double)(sq(s[j]) - sq(s[k]) + dp[j] - dp[k]) / (s[j] - s[k]); }
i64 f(int i) { return 2 * (s[i] - L); }

int main()
{
   int n, i, j, k, opt;
   int fr, bk;
   int len;

   // Input & preprocessing
   scanf("%d", &n);
   s[0] = 0;
   for (i = 1; i <= n; i++)
   {
      scanf("%lld", &x[i]);
      s[i] = s[i-1] + x[i] + 1;
   }

   // Segment Queue
   dp[0] = 0;
   fr = bk = 0;
   que[bk++] = 0;
   for (i = 1; i <= n; i++)
   {
      while (fr + 1 < bk && f(i) >= delta(que[fr], que[fr+1]))
         fr++;
      opt = pi[i] = que[fr];
      dp[i] = sq(s[i] - s[opt] - L) + dp[opt];
      while (fr + 1 < bk && delta(que[bk-2], que[bk-1]) >= delta(que[bk-1], i))
         bk--;
      que[bk++] = i;
   }

   // Reconstruct Partition
   len = 0;
   parts[len++] = n;
   while (parts[len-1])
   {
      parts[len] = pi[parts[len-1]];
      len++;
   }

   // Output
   printf("[");
   for (i = len - 1; i > 0; i--)
   {
      printf("[");
      for (j = parts[i] + 1; j < parts[i-1]; j++)
         printf("%lld,", x[j]);
      printf("%lld]", x[j]);
      if (i > 1)
         printf(",");
   }
   printf("]\n");
   printf("%lld\n", dp[n]);
   
   return 0;
}
