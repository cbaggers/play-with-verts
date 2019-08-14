(in-package #:play-with-verts)

;; Texture2D inputRT : register(t0);
;; RWTexture2D outputRT : register(u0);
;; StructuredBuffer instanceDataIn : register(t1);
;; AppendStructuredBuffer instanceDataOut : register(u0);
;; RWBuffer instanceCounts : register(u1);
;; SamplerState samplerPoint : register(s0);
;;
;; void occlusion(uint3 threadID : SV_DispatchThreadID)
;; {
;;     //make sure that we not processing more instances than available
;;     if (threadID.x < NoofInstances)
;;     {
;;         float3 bboxMin = instanceDataIn[threadID.x].bboxMin.xyz;
;;         float3 bboxMax = instanceDataIn[threadID.x].bboxMax.xyz;
;;         float3 boxSize = bboxMax - bboxMin;
;;
;;         float3 boxCorners[] = { bboxMin.xyz,
;;                                 bboxMin.xyz + float3(boxSize.x,0,0),
;;                                 bboxMin.xyz + float3(0, boxSize.y,0),
;;                                 bboxMin.xyz + float3(0, 0, boxSize.z),
;;                                 bboxMin.xyz + float3(boxSize.xy,0),
;;                                 bboxMin.xyz + float3(0, boxSize.yz),
;;                                 bboxMin.xyz + float3(boxSize.x, 0, boxSize.z),
;;                                 bboxMin.xyz + boxSize.xyz
;;                              };
;;         float minZ = 1;
;;         float2 minXY = 1;
;;         float2 maxXY = 0;
;;
;;         [unroll]
;;         for (int i = 0; i < 8; i++)
;;         {
;;             //transform world space aaBox to NDC
;;             float4 clipPos = mul(float4(boxCorners[i], 1), ViewProjection);
;;
;;             clipPos.z = max(clipPos.z, 0);
;;
;;             clipPos.xyz = clipPos.xyz / clipPos.w;
;;
;;             clipPos.xy = clamp(clipPos.xy, -1, 1);
;;             clipPos.xy = clipPos.xy * float2(0.5, -0.5) + float2(0.5, 0.5);
;;
;;             minXY = min(clipPos.xy, minXY);
;;             maxXY = max(clipPos.xy, maxXY);
;;
;;             minZ = saturate(min(minZ, clipPos.z));
;;         }
;;
;;         float4 boxUVs = float4(minXY, maxXY);
;;
;;         // Calculate hi-Z buffer mip
;;         int2 size = (maxXY - minXY) * RTSize.xy;
;;         float mip = ceil(log2(max(size.x, size.y)));
;;
;;         mip = clamp(mip, 0, MaxMipLevel);
;;
;;         // Texel footprint for the lower (finer-grained) level
;;         float  level_lower = max(mip - 1, 0);
;;         float2 scale = exp2(-level_lower);
;;         float2 a = floor(boxUVs.xy*scale);
;;         float2 b = ceil(boxUVs.zw*scale);
;;         float2 dims = b - a;
;;
;;         // Use the lower level if we only touch <= 2 texels in both dimensions
;;         if (dims.x <= 2 && dims.y <= 2)
;;             mip = level_lower;
;;
;;         //load depths from high z buffer
;;         float4 depth = { inputRT.SampleLevel(samplerPoint, boxUVs.xy, mip),
;;                          inputRT.SampleLevel(samplerPoint, boxUVs.zy, mip),
;;                          inputRT.SampleLevel(samplerPoint, boxUVs.xw, mip),
;;                          inputRT.SampleLevel(samplerPoint, boxUVs.zw, mip)
;;                         };
;;
;;         //find the max depth
;;         float maxDepth = max(max(max(depth.x, depth.y), depth.z), depth.w);
;;
;;         if (ActivateCulling == 0 || minZ <= maxDepth)
;;         {
;;             //instance is visible, add it to the buffer
;;             instanceDataOut.Append(instanceDataIn[threadID.x].world);
;;
;;             InterlockedAdd(instanceCounts[1], 1);
;;         }
;;     }
;; }

(defstruct-g (ssbo-test-data :layout std-430)
  (vals (:int 100)))

(defun-g test-compute-func (&uniform (woop ssbo-test-data :ssbo))
  (declare (local-size :x 1 :y 1 :z 1))
  (setf (aref (ssbo-test-data-vals woop)
              (int (x gl-work-group-id)))
        (int (x gl-work-group-id)))
  (values))

(defpipeline-g test-compute-pline ()
  :compute test-compute-func)

(defun test-compute ()
  (let* ((data (make-gpu-array nil :dimensions 1
                               :element-type 'ssbo-test-data))
         (ssbo (make-ssbo data)))
    (unwind-protect
         (progn
           (map-g #'test-compute-pline (make-compute-space 100)
                  :woop ssbo)
           (wait-on-gpu-fence (make-gpu-fence))
           (pull-g ssbo))
      (free ssbo)
      (free data))))
