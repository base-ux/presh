#!/bin/sh

# Set variables
PROG="$(basename -- "$0")"

D="$(dirname -- "$0")"
D="$(cd -- "${D}" ; pwd)"

SRCDIR="${D}"
OUTDIR="${D}/out"

PRESH_AWK="${SRCDIR}/presh.awk"
PRESH_SHT="${SRCDIR}/presh.sht"
PRESH_SH="${OUTDIR}/presh.sh"

AWK="$(command -pv awk)"
CHMOD="$(command -pv chmod)"

####

# Print error message
err ()
{
    ! printf "%s: error: %s\n" "${PROG}" "$*" >&2
}

# Main subroutine
main ()
{
    # Check executables
    test -n "${AWK}"   || err "can't find 'awk'"   || return 1
    test -n "${CHMOD}" || err "can't find 'chmod'" || return 1
    # Create output directory
    test -d "${OUTDIR}" || mkdir -p "${OUTDIR}" || return 1
    # Build presh.sh
    "${AWK}" -f "${PRESH_AWK}" "${PRESH_SHT}" > "${PRESH_SH}" || return 1
    # Make presh.sh executable
    "${CHMOD}" +x "${PRESH_SH}" || return 1
}

# Call main subroutine
main "$@"
