;;; moonrise.el --- calculate moonrise/moonset      -*- lexical-binding: t; -*-

;; Copyright (C) 2020  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: calendar
;; Human-Keywords: moonrise, moonset, moon, lunar phases, calendar, diary, org

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; References:

;; [長沢99] 長沢 工. 日の出・日の入りの計算. 地人書館, 1999, ISBN978-4805206348

;;; Code:

(require 'solar)
(require 'svg)

;;
;; Settings
;;
(defcustom moonrise-point-name-alist
  '((rise . "Moonrise")
    (set . "Moonset")
    (meridian . "Moon's Meridian Passing"))
  "Alist of moon position names."
  :type '(alist :key-type (symbol :tag "Key")
                :key-value (string :tag "Value"))
  :group 'calendar)

(defcustom moonrise-display-points
  '(rise meridian set)
  "List of points display time."
  :type '(list symbol)
  :group 'calendar)

(defcustom moonrise-display-points-org-agenda
  '(rise set)
  "List of points display time for org-agenda."
  :type '(list symbol)
  :group 'calendar)

(defcustom moonrise-moon-age-format
  " (%.2f)"
  "Moon's age format."
  :type '(choice (string :tag "Format string")
                 (function :tag "Formatter"))
  :group 'calendar)

(defcustom moonrise-moon-age-display-points
  '(meridian)
  "List of points display moon's age."
  :type '(list symbol)
  :group 'calendar)

(defcustom moonrise-moon-age-display-points-org-agenda
  '(rise)
  "List of points display moon's age for org-agenda."
  :type '(list symbol)
  :group 'calendar)

(defcustom moonrise-moon-phase-format
  #'moonrise-moon-phase-format-default
  "Moon phase format."
  :type '(choice (string :tag "Format string")
                 (function :tag "Formatter"))
  :group 'calendar)

(defcustom moonrise-moon-phase-display-points
  '(meridian)
  "List of points display moon phase."
  :type '(list symbol)
  :group 'calendar)

(defcustom moonrise-moon-phase-display-points-org-agenda
  '(rise)
  "List of points display moon phase for org-phasenda."
  :type '(list symbol)
  :group 'calendar)


;;
;; Math Utilities
;;

(defun moonrise-normalize-degrees (deg)
  (mod deg 360))

(defun moonrise-normalize-degrees-180+180 (deg)
  (let ((nd (mod deg 360)))
    (if (> nd 180)
        (+ nd -360)
      nd)))

(defun moonrise-sin (deg)
  (sin (degrees-to-radians (mod deg 360.0))))

(defun moonrise-cos (deg)
  (cos (degrees-to-radians (mod deg 360.0))))

;;
;; Calculate Moon Position
;;
;; see: [長沢99] 5.4 表によらない月位置
;;

(defun moonrise-a*sin{b+c*T} (a b c T)
  (* a (moonrise-sin (+ b (* c T)))))

(defun moonrise-sum{a*sin{b+c*T}} (list-abc T)
  (let ((sum 0))
    (while list-abc
      (cl-incf sum (moonrise-a*sin{b+c*T}
                    (pop list-abc)
                    (pop list-abc)
                    (pop list-abc)
                    T)))
    sum))

(defun moonrise-A_m (T)
  (moonrise-sum{a*sin{b+c*T}}
   '(0.0040 119.5 1.33
            0.0020 55.0 19.34
            0.0006 71 0.2
            0.0006 54 19.3)
   T))

(defun moonrise-ecliptic-longitude (T)
  (+ 218.3161 (* 4812.67881 T)
     (moonrise-a*sin{b+c*T} 6.2887 (+ 134.961 (moonrise-A_m T)) 4771.9886 T)
     (moonrise-sum{a*sin{b+c*T}}
      '(1.2740 100.738 4133.3536
               0.6583 235.700 8905.3422
               0.2136 269.926 9543.9773
               0.1856 177.525 359.9905
               0.1143 6.546 9664.0404
               0.0588 214.22 638.635
               0.0572 103.21 3773.363
               0.0533 10.66 13677.331
               0.0459 238.18 8545.352
               0.0410 137.43 4411.998
               0.0348 117.84 4452.671
               0.0305 312.49 5131.979
               0.0153 130.84 758.698
               0.0125 141.51 14436.029
               0.0110 231.59 4892.052
               0.0107 336.44 13038.696
               0.0100 44.89 14315.966
               0.0085 201.5 8266.71
               0.0079 278.2 4493.34
               0.0068 53.2 9265.33
               0.0052 197.2 319.32
               0.0050 295.4 4812.66
               0.0048 235.0 19.34
               0.0040 13.2 13317.34
               0.0040 145.6 18449.32
               0.0040 119.5 1.33
               0.0039 111.3 17810.68
               0.0037 349.1 5410.62
               0.0027 272.5 9183.99
               0.0026 107.2 13797.39
               0.0024 211.9 988.63
               0.0024 252.8 9224.66
               0.0022 240.6 8185.36
               0.0021 87.5 9903.97
               0.0021 175.1 719.98
               0.0021 105.6 3413.37
               0.0020 55.0 19.34
               0.0018 4.1 4013.29
               0.0016 242.2 18569.38
               0.0012 339.0 12678.71
               0.0011 276.5 19208.02
               0.0009 218 8586.0
               0.0008 188 14037.3
               0.0008 204 7906.7
               0.0007 140 4052.0
               0.0007 275 4853.3
               0.0007 216 278.6
               0.0006 128 1118.7
               0.0005 247 22582.7
               0.0005 181 19088.0
               0.0005 114 17450.7
               0.0005 332 5091.3
               0.0004 313 398.7
               0.0004 278 120.1
               0.0004 71 9584.7
               0.0004 20 720.0
               0.0003 83 3814.0
               0.0003 66 3494.7
               0.0003 147 18089.3
               0.0003 311 5492.0
               0.0003 161 40.7
               0.0003 280 23221.3)
      T)))

(defun moonrise-B_m (T)
  (moonrise-sum{a*sin{b+c*T}}
   '(0.0267 234.95 19.341
            0.0043 322.1 19.36
            0.0040 119.5 1.33
            0.0020 55.0 19.34
            0.0005 307 19.4)
   T))

(defun moonrise-ecliptic-latitude (T)
  (+ (moonrise-a*sin{b+c*T} 5.1282 (+ 93.273 (moonrise-B_m T)) 4832.0202 T)
     (moonrise-sum{a*sin{b+c*T}}
      '(0.2806 228.235 9604.0088
               0.2777 138.311 60.0316
               0.1732 142.427 4073.3220
               0.0554 194.01 8965.374
               0.0463 172.55 698.667
               0.0326 328.96 13737.362
               0.0172 3.18 14375.997
               0.0093 277.4 8845.31
               0.0088 176.7 4711.96
               0.0082 144.9 3713.33
               0.0043 307.6 5470.66
               0.0042 103.9 18509.35
               0.0034 319.9 4433.31
               0.0025 196.5 8605.38
               0.0022 331.4 13377.37
               0.0021 170.1 1058.66
               0.0019 230.7 9244.02
               0.0018 243.3 8206.68
               0.0018 270.8 5192.01
               0.0017 99.8 14496.06
               0.0016 135.7 420.02
               0.0015 211.1 9284.69
               0.0015 45.8 9964.00
               0.0014 219.2 299.96
               0.0013 95.8 4472.03
               0.0013 155.4 379.35
               0.0012 38.4 4812.68
               0.0012 148.2 4851.36
               0.0011 138.3 19147.99
               0.0010 18.0 12978.66
               0.0008 70 17870.7
               0.0008 326 9724.1
               0.0007 294 13098.7
               0.0006 224 5590.7
               0.0006 52 13617.3
               0.0005 280 8485.3
               0.0005 239 4193.4
               0.0004 311 9483.9
               0.0004 238 23281.3
               0.0004 81 10242.6
               0.0004 13 9325.4
               0.0004 147 14097.4
               0.0003 205 22642.7
               0.0003 107 18149.4
               0.0003 146 3353.3
               0.0003 234 19268.0)
      T)))

(defun moonrise-parallax (T)
  (+ 0.9507 ;;*(sin(90deg))
     (moonrise-sum{a*sin{b+c*T}}
      '(0.0518 224.98 4771.989
               0.0095 190.7 4133.35
               0.0078 325.7 8905.34
               0.0028 0.0 9543.98
               0.0009 100.0 13677.3
               0.0005 329 8545.4
               0.0004 194 3773.4
               0.0003 227 4412.0)
      T)))

(defun moonrise-ecliptic-obliquity (T)
  (- 23.439291 (* 0.000130042 T)))

(defun moonrise-ecliptic-to-equatorial (long lat obliquity)
  (let* ((cos_long (moonrise-cos long))
         (sin_long (moonrise-sin long))
         (cos_lat (moonrise-cos lat))
         (sin_lat (moonrise-sin lat))
         (cos_obliquity (moonrise-cos obliquity))
         (sin_obliquity (moonrise-sin obliquity))
         (u (* cos_lat cos_long))
         (v (- (* cos_lat sin_long cos_obliquity)
               (* sin_lat sin_obliquity)))
         (w (+ (* cos_lat sin_long sin_obliquity)
               (* sin_lat cos_obliquity))))
     (list
       (mod (radians-to-degrees (atan v u)) 360)
       (radians-to-degrees (atan w (sqrt (+ (* u u) (* v v))))))))


;;
;; Time Conversion
;;
;; - jd2000 : Julian Days from J2000.0
;; - local-date : calendar date in Local Time
;; - utdate : calendar date in Universal Time
;; - time : emacs time

;; Calendar Date in Universal Time

(defun moonrise-jd2000-from-utdate (date &optional hour min sec)
  (+ (- (calendar-absolute-from-gregorian date)
        (calendar-absolute-from-gregorian '(1 1.5 2000)))
     (if hour (/ hour 24.0) 0)
     (if min (/ min 1440.0) 0) ;;24*60=1440
     (if sec (/ sec 86400.0) 0))) ;;24*60*60=86400

(defun moonrise-utdate-from-jd2000 (jd2000)
  (let* ((frac (mod (- jd2000 0.5) 1.0))
         (date (calendar-gregorian-from-absolute
                (floor (+ (calendar-absolute-from-gregorian '(1 1.5 2000)) jd2000)))))
    (if (not (= frac 0))
        (cl-incf (cadr date) frac))
    date))

;; Calendar Date in Local Time

(defun moonrise-jd2000-from-local-date (date)
  (moonrise-jd2000-from-utdate date nil (- calendar-time-zone)))

(defun moonrise-local-date-from-jd2000 (jd2000)
  (moonrise-utdate-from-jd2000 (+ jd2000 (/ calendar-time-zone (* 24 60.0)))))

;; Astronomical Julian Day

(defun moonrise-jd2000-from-local-astro (astro)
  (- astro
     2451545.0
     (/ calendar-time-zone (* 60.0 24.0))))

(defun moonrise-local-astro-from-jd2000 (jd2000)
  (+ jd2000
     2451545.0
     (/ calendar-time-zone (* 60.0 24.0))))

;; Emacs Time

(defun moonrise-time-from-jd2000 (jd2000)
  (if jd2000
      (let* ((jd-date-time (moonrise-jd-to-date-time jd2000))
             (jd-date (car jd-date-time))
             (jd-time (cdr jd-date-time)))
        (encode-time (round (* jd-time 86400)) 0 0
                     (floor (+ jd-date 1.5)) 1 2000 t))))

(defun moonrise-jd2000-from-time (time)
  (if time
      (let* ((tm (decode-time time t))
             (sec (nth 0 tm))
             (min (nth 1 tm))
             (hour (nth 2 tm))
             (day (nth 3 tm))
             (month (nth 4 tm))
             (year (nth 5 tm)))
        (moonrise-jd2000-from-utdate (list month day year) hour min sec))))

;; Utilities

(defun moonrise-floor-date (date)
  "Discard fraction part of calendar DATE."
  (list (floor (car date)) (floor (cadr date)) (floor (caddr date))))

(defun moonrise-jd-to-date-time (jd)
  "Divide julian day JD into a julian day and a fraction part."
  (let* ((date-part (- (floor (+ jd 0.5)) 0.5))
         (time-part (- jd date-part)))
    (cons date-part time-part)))

(defun moonrise-jd-to-jc (jd)
  (/ jd 36525.0))

(defun moonrise-jd-to-jy (jd)
  (/ jd 365.25))

(defun moonrise-jy-to-jd (jy)
  (* jy 365.25))

;; Sidereal Time

(defun moonrise-sidereal-time-from-jd2000 (jd2000)
  (let ((jd-date-time (moonrise-jd-to-date-time jd2000)))
    (mod
     (+ (* (solar-sidereal-time (moonrise-jd-to-jc (car jd-date-time))) 15)
        (* (cdr jd-date-time) 360 1.00273790701))
     360)))


;;
;; Time Finding
;;
;; [長沢99] 5.3

(defun moonrise-around (jd2000 &optional target-point long lat height)
  "Find the time when moon passes TARGET-POINT before or after JD2000."
  (if (null target-point) (setq target-point 'rise))
  (if (null long) (setq long (calendar-longitude)))
  (if (null lat) (setq lat (calendar-latitude)))
  (if (null height) (setq height 0))

  (let* ((jd0 jd2000)
         (et (solar-ephemeris-correction (caddr (moonrise-utdate-from-jd2000 jd0))))
         (st0 (moonrise-normalize-degrees (+ (moonrise-sidereal-time-from-jd2000 jd0) long)))
         (horizontal-refraction 0.585556)
         (dip (* 0.0353333333333 (sqrt height)))
         (d 0)
         (delta-d 1))
    (while (>= (abs delta-d) 0.000005)
      (let* ((jy (moonrise-jd-to-jy (+ jd0 et d)))
             ;; calculate moon position
             (ra-dec (moonrise-ecliptic-to-equatorial
                      (moonrise-ecliptic-longitude jy)
                      (moonrise-ecliptic-latitude jy)
                      (moonrise-ecliptic-obliquity jy)))
             (ra (car ra-dec)) ;;alpha
             (dec (cadr ra-dec)) ;;delta
             (para (moonrise-parallax jy)) ;;pi
             (st (+ st0 (* 360.9856474 d))) ;;theta (sidereal time)
             (moon-hour-angle (- st ra)) ;;t

             ;; calculate rising or setting point
             (rise-set-altitude (- para horizontal-refraction dip)) ;;k see:p.20
             (cos-rise-set-hour-angle (/ (- (moonrise-sin rise-set-altitude) (* (moonrise-sin dec) (moonrise-sin lat)))
                                         (* (moonrise-cos dec) (moonrise-cos lat)))) ;;cos(t_k) see:p.38
             (rise-set-hour-angle (radians-to-degrees (acos cos-rise-set-hour-angle)))

             ;; target point
             (target-point-hour-angle ;;-t_k (hour angle of rise or set point)
              (cond
               ((eq target-point 'meridian) 0) ;;@todo do not calculate rise-set-hour-angle
               ((eq target-point 'set) rise-set-hour-angle)
               (t (- rise-set-hour-angle))))

             ;; angle moon to target point
             (angle-moon-to-target (moonrise-normalize-degrees-180+180 (- target-point-hour-angle moon-hour-angle))))

        (setq delta-d (/ angle-moon-to-target 347.8))
        ;;(message "d=%f ra=%f dec=%f st=%f para=%f rise-set-alt=%f cos-rise-set-hangle=%f target-point-hangle=%f moon-hangle=%f delta-angle=%f delta-d=%f" d ra dec st para rise-set-altitude cos-rise-set-hour-angle target-point-hour-angle moon-hour-angle angle-moon-to-target delta-d)
        (setq d (+ d delta-d))))

    (+ jd2000 d)))

(defun moonrise-in-day (day-begin-jd2000 &optional target-point long lat height)
  (let ((jdp (moonrise-around (+ day-begin-jd2000 0.5) target-point long lat height)))
    (if (and (>= jdp day-begin-jd2000) (< jdp (+ day-begin-jd2000 1.0)))
        jdp)))

(defun moonrise-list (begin-jd2000 end-jd2000 &optional target-point long lat height)
  (let ((jd begin-jd2000)
        results)
    (while (< jd end-jd2000)
      (let ((jdp (moonrise-in-day jd target-point long lat height)))
        (if (and jdp (not (and results (< (abs (- (car results) jdp)) 1e-6))))
            (push jdp results)))
      (setq jd (+ jd 1.0)))
    (nreverse results)))



;; Ex: calculate moonrise around specified time
;; (let* ((jd0 (moonrise-jd2000-from-utdate '(6 23 2020) -9))
;;        (jdp (moonrise-around (+ jd0 0.0) 'rise))
;;        (d (- jdp jd0)))
;;     (message "%02d:%02d:%02d"
;;             (floor (* 24 d))
;;             (floor (mod (* 24 60 d) 60))
;;             (floor (mod (* 24 60 60 d) 60))))

;; Ex: calculate moonrise around specified range
;; (mapconcat
;;  (lambda (jd2000) (format-time-string "%FT%T%z" (moonrise-time-from-jd2000 jd2000)))
;;  (moonrise-list (moonrise-jd2000-from-utdate '(6 23 2020) -9)
;;                 (moonrise-jd2000-from-utdate '(6 30 2020) -9) 'rise)
;;  "\n")


;;
;; Moon's Age
;;
;; [長沢99] 5.6

(defun moonrise-moon-age (jd2000)
  (let* ((jd0 jd2000)
         (et (solar-ephemeris-correction (caddr (moonrise-utdate-from-jd2000 jd0))))
         (jd jd0)
         (delta-long 1)
         (num-iter 0))

    (while (>= (abs delta-long) 0.05)
      (let* ((solar-long (solar-longitude (moonrise-local-astro-from-jd2000 jd)))
             (lunar-long (moonrise-normalize-degrees
                          (moonrise-ecliptic-longitude
                           (moonrise-jd-to-jy (+ jd et)))))
             (solar-to-lunar-angle (funcall
                                    (if (= num-iter 0)
                                        #'moonrise-normalize-degrees
                                      #'moonrise-normalize-degrees-180+180)
                                    (- lunar-long solar-long)))
             (age (/ solar-to-lunar-angle 12.1908))
             (new-moon (- jd age)))
        ;;(message "jd=%f delta-long=%f age=%f new-moon=%f" jd delta-long age new-moon)
        (setq jd new-moon)
        (setq delta-long solar-to-lunar-angle)
        (setq num-iter (1+ num-iter))))

     (- jd0 jd)))

;;(moonrise-moon-age (moonrise-jd2000-from-local-date (list 6 21.5 2020))) => 29.390037587843835
;;(moonrise-moon-age (moonrise-jd2000-from-local-date (list 6 (+ 25 (/ 8 24)) 2020)))
;;(moonrise-moon-age (moonrise-jd2000-from-time (encode-time 0 0 8 25 6 2020)))


;;
;; Moon Phase
;;

(defun moonrise-moon-phase (jd2000)
  (let* ((et (solar-ephemeris-correction (caddr (moonrise-utdate-from-jd2000 jd2000))))
         (solar-long (solar-longitude (moonrise-local-astro-from-jd2000 jd2000)))
         (lunar-long (moonrise-normalize-degrees
                      (moonrise-ecliptic-longitude
                       (moonrise-jd-to-jy (+ jd2000 et)))))
         (solar-to-lunar-angle (moonrise-normalize-degrees
                                (- lunar-long solar-long))))
    solar-to-lunar-angle))

;;(moonrise-moon-phase (moonrise-jd2000-from-time (encode-time 0 37 11 24 7 2021))) => 180.???

(defun moonrise-moon-phase-format-default (phase)
  (concat " " (moonrise-moon-phase-svg-image-string phase (1- (default-font-height)))))

(defun moonrise-moon-phase-svg-image-string (phase size &optional ascent light shadow)
  (let ((str (format "%.2f[deg]" phase)))
    (if (and
         (featurep 'image)
         (featurep 'svg)
         (display-graphic-p)
         (fboundp 'image-type-available-p)
         (image-type-available-p 'svg))
        (propertize str
                    'display (moonrise-moon-phase-svg-image phase size ascent light shadow))
      str)))

(defun moonrise-moon-phase-svg-image (phase size &optional ascent light shadow)
  (svg-image
   (moonrise-moon-phase-svg phase size light shadow)
   :ascent (or ascent 'center)))

(defun moonrise-moon-phase-svg (phase size &optional light shadow)
  (let* ((svg (svg-create size size))
         ;; center and radius
         (cx (* 0.5 size))
         (cy (* 0.5 size))
         (radius (* 0.48 size))
         ;; number of divisions
         (ndiv 32)
         (2pi/ndiv (* 2.0 (/ pi ndiv)))
         ;; degrees to radians
         (phase-rad (* 2.0 pi (/ (mod phase 360.0) 360.0)))
         ;; left and right sides of sunlit portion
         (right-edge (if (<= phase-rad pi) 1.0 (- (cos phase-rad))))
         (left-edge (if (>= phase-rad pi) 1.0 (- (cos phase-rad)))))
    ;; entire of moon
    (svg-circle svg cx cy radius :fill (or shadow "#000"))
    ;; sunlit portion
    (svg-polygon
     svg
     (cl-loop for i from 0 to ndiv
              collect (let ((i-rad (* i 2pi/ndiv))
                            (edge (if (< (* 2 i) ndiv) right-edge left-edge)))
                        (cons (+ cx (* (sin i-rad) radius edge))
                              (- cy (* (cos i-rad) radius)))))
     :fill (or light "#ffc"))
    svg))


;;
;; Formatting
;;

(defun moonrise-point-name (target-point)
  (or (cdr (assq target-point moonrise-point-name-alist)) ""))

(defun moonrise-moon-age-string (age)
  (cond
   ((stringp moonrise-moon-age-format)
    (format moonrise-moon-age-format age))
   ((functionp moonrise-moon-age-format)
    (funcall moonrise-moon-age-format age))))

(defun moonrise-moon-phase-string (phase)
  (cond
   ((stringp moonrise-moon-phase-format)
    (format moonrise-moon-phase-format phase))
   ((functionp moonrise-moon-phase-format)
    (funcall moonrise-moon-phase-format phase))))

(defun moonrise-time-string (time)
  (if time
      (let* ((tm (decode-time time (* calendar-time-zone 60)))
             (min (nth 1 tm))
             (hour (nth 2 tm)))
        (solar-time-string (+ hour (/ min 60.0)) calendar-standard-time-zone-name))))

(defun moonrise-time-and-point-string (time target-point moon-age-p &optional moon-phase-p)
  (if time
      (concat
       (moonrise-point-name target-point)
       " "
       (moonrise-time-string time)

       (if moon-age-p
           (moonrise-moon-age-string (moonrise-moon-age (moonrise-jd2000-from-time time))))

       (if moon-phase-p
           (moonrise-moon-phase-string (moonrise-moon-phase (moonrise-jd2000-from-time time)))))))

(defun moonrise-moonset-string (local-date  &optional separator display-points moon-age-display-points moon-phase-display-points)
  (if (null display-points) (setq display-points moonrise-display-points))
  (if (null moon-age-display-points) (setq moon-age-display-points moonrise-moon-age-display-points))
  (if (null moon-phase-display-points) (setq moon-phase-display-points moonrise-moon-phase-display-points))

  (let ((day-begin (moonrise-jd2000-from-local-date local-date)))
    (mapconcat
     (lambda (jdtp)
       (moonrise-time-and-point-string
        (moonrise-time-from-jd2000 (car jdtp))
        (cdr jdtp)
        (memq (cdr jdtp) moon-age-display-points)
        (memq (cdr jdtp) moon-phase-display-points)))
     (sort
      (delq nil
            (mapcar (lambda (target-point)
                      (let ((jd (moonrise-in-day day-begin target-point)))
                        (if jd (cons jd target-point))))
                    display-points))
      (lambda (jdtp1 jdtp2) (< (car jdtp1) (car jdtp2))))
     (or separator ", "))))

;;
;; for calendar, diary, org-mode
;;

;; for calendar

;;;###cal-autoload
(defun calendar-moonrise-moonset (&optional event)
  "Local time of moonrise and moonset for date under cursor.
Accurate to a few seconds."
  (interactive (list last-nonmenu-event))
  (or (and calendar-latitude calendar-longitude calendar-time-zone)
      (solar-setup))
  (let ((local-date (calendar-cursor-to-date t event)))
    (message "%s: %s"
             (calendar-date-string local-date t t)
             (moonrise-moonset-string local-date))))

(defconst moonrise-moonrises-buffer "*Moonrise/Moonset Times*"
  "Name of buffer used for moonrise/moonset times.")

;;;###cal-autoload
(defun calendar-moonrise-moonset-month (&optional event)
  "Local time of moonrise and moonset for month under cursor or at EVENT."
  (interactive (list last-nonmenu-event))
  (or (and calendar-latitude calendar-longitude calendar-time-zone)
      (solar-setup))
  (let* ((date (calendar-cursor-to-date t event))
         (month (car date))
         (year (nth 2 date))
         (last (calendar-last-day-of-month month year))
         (title (format "Moonrise/moonset times for %s %d at %s"
                        (calendar-month-name month) year
                        (eval calendar-location-name))))
    (calendar-in-read-only-buffer moonrise-moonrises-buffer
      (calendar-set-mode-line title)
      (insert title ":\n\n")
      (dotimes (i last)
        (setq date (list month (1+ i) year))
        (insert (format "%s %2d: " (calendar-month-name month t) (1+ i))
                (moonrise-moonset-string date) "\n")))))

;; for diary
;;(with-no-warnings (defvar date))
;;;###diary-autoload
(defun diary-moonrise-moonset ()
  "Local time of moonrise and moonset as a diary entry.
Accurate to a few seconds."
  (or (and calendar-latitude calendar-longitude calendar-time-zone)
      (solar-setup))
  (moonrise-moonset-string date))

;; for org-agenda

(defcustom moonrise-org-agenda-use-cache nil ""
  :type 'boolean
  :group 'calendar)

(with-no-warnings (defvar org-agenda-current-date))

(defun moonrise-org-agenda ()
  (if moonrise-org-agenda-use-cache
      (let ((key (moonrise-org-agenda-cache-key)))
        (or (moonrise-org-agenda-cache-get key)
            (moonrise-org-agenda-cache-put key
                                           (moonrise-org-agenda-make-string))))
    (moonrise-org-agenda-make-string)))
;;(moonrise-org-agenda)

(defun moonrise-org-agenda-make-string ()
  (moonrise-moonset-string
   org-agenda-current-date
   "; "
   moonrise-display-points-org-agenda
   moonrise-moon-age-display-points-org-agenda
   moonrise-moon-phase-display-points-org-agenda))

(defvar moonrise-org-agenda-cache nil)

(defun moonrise-org-agenda-cache-clear ()
  (interactive)
  (setq moonrise-org-agenda-cache nil))

(defun moonrise-org-agenda-cache-key ()
  (let ((d org-agenda-current-date))
    (intern (format "%04d%02d%02d" (nth 2 d) (nth 0 d) (nth 1 d)))))

(defun moonrise-org-agenda-cache-get (key)
  (cdr (assq key moonrise-org-agenda-cache)))

(defun moonrise-org-agenda-cache-put (key str)
  (push (cons key str) moonrise-org-agenda-cache)
  str)

(provide 'moonrise)
;;; moonrise.el ends here
