
plot(log(all$q_np_det_joint_cap+all$q_np_det_joint_nocap)~log(all$q_np_det_sep_cap+all$q_np_det_sep_nocap),
     pch=20, col="darkblue",
     main="NPs with Det and Prp+Det (clitic) and no Det", xlab="Separate (capitalised)", ylab="Joint (capitalised)")
points(log(all$q_np_apprart_joint_cap+all$q_np_apprart_joint_nocap)~log(all$q_np_apprart_sep_cap+all$q_np_apprart_sep_nocap),
       pch=18, col="darkorange")
points(log(all$q_np_nodet_joint_cap+all$q_np_nodet_joint_nocap)~log(all$q_np_nodet_sep_cap+all$q_np_nodet_sep_nocap),
       pch=19, col="lightgreen", cex=0.5)

plot(log(all$q_particip_joint_cap+all$q_particip_joint_nocap)~log(all$q_particip_sep_cap+all$q_particip_sep_nocap),
     pch=20, col="darkgreen",
     main="am Progressives", xlab="Separate (capitalised)", ylab="Joint (capitalised)")
points(log(all$q_prog_joint_cap+all$q_prog_joint_nocap)~log(all$q_prog_sep_cap+all$q_prog_sep_nocap),
       pch=20, col="darkorange",
       main="am Progressives", xlab="Separate (capitalised)", ylab="Joint (capitalised)")
points(log(all$q_infzu_joint_cap+all$q_infzu_joint_nocap)~log(all$q_infzu_sep_cap+all$q_infzu_sep_nocap),
       pch=20, col="blue",
       main="zu Infinitives", xlab="Separate (capitalised)", ylab="Joint (capitalised)")

plot(log(all$q_finite_joint_cap+all$q_finite_joint_nocap)~log(all$q_finite_sep_cap+all$q_finite_sep_nocap),
     pch=20, col="darkblue",
     main="Finite", xlab="Separate (capitalised)", ylab="Joint (capitalised)")

