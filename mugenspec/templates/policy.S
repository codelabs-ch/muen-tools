.global kernel_cluster_count
.global kernel_cpu_count
.global kernel_translation_tables
.global gic_physical_addresses
.global smmu_physical_address

.section   .globalrodata

.align 8

kernel_cluster_count:
__kernel_cluster_count__

kernel_cpu_count:
__kernel_cpu_count__

kernel_translation_tables:
__kernel_translation_tables__

gic_physical_addresses:
__gic_physical_addresses__

smmu_physical_address:
__smmu_physical_address__
