# License Registry

**Version**: 1.0  
**Last Updated**: 2025-11-06  
**Author**: WORKER wrk-12 - Compliance & Governance

## Project License

**Primary License**: Apache License 2.0  
**SPDX Identifier**: `Apache-2.0`  
**License File**: `LICENSE`

## Allowed Licenses (Allowlist)

The following license types are **allowed** for dependencies:

### Permissive Licenses (Fully Compatible)

- **MIT** (SPDX: `MIT`) - https://spdx.org/licenses/MIT.html
- **Apache-2.0** (SPDX: `Apache-2.0`) - https://spdx.org/licenses/Apache-2.0.html
- **BSD-2-Clause** (SPDX: `BSD-2-Clause`) - https://spdx.org/licenses/BSD-2-Clause.html
- **BSD-3-Clause** (SPDX: `BSD-3-Clause`) - https://spdx.org/licenses/BSD-3-Clause.html
- **ISC** (SPDX: `ISC`) - https://spdx.org/licenses/ISC.html
- **Unlicense** (SPDX: `Unlicense`) - https://spdx.org/licenses/Unlicense.html

### Weak Copyleft Licenses (Compatible with Conditions)

- **LGPL-2.1** (SPDX: `LGPL-2.1`) - https://spdx.org/licenses/LGPL-2.1-only.html - Requires library use
- **LGPL-3.0** (SPDX: `LGPL-3.0`) - https://spdx.org/licenses/LGPL-3.0-only.html - Requires library use
- **MPL-2.0** (SPDX: `MPL-2.0`) - https://spdx.org/licenses/MPL-2.0.html - File-level copyleft

## Prohibited Licenses (Denylist)

The following license types are **prohibited** for dependencies:

- **GPL-2.0** (SPDX: `GPL-2.0`) - https://spdx.org/licenses/GPL-2.0-only.html - Strong copyleft
- **GPL-3.0** (SPDX: `GPL-3.0`) - https://spdx.org/licenses/GPL-3.0-only.html - Strong copyleft
- **AGPL-3.0** (SPDX: `AGPL-3.0`) - https://spdx.org/licenses/AGPL-3.0-only.html - Network copyleft
- **SSPL** (SPDX: `SSPL`) - https://spdx.org/licenses/SSPL-1.0.html - Restrictive
- **BUSL-1.1** (SPDX: `BUSL-1.1`) - https://spdx.org/licenses/BUSL-1.1.html - Business Source License

## License Compatibility Matrix

| Dependency License | Project License (Apache-2.0) | Status |
|-------------------|------------------------------|--------|
| MIT | Apache-2.0 | ✅ Compatible |
| Apache-2.0 | Apache-2.0 | ✅ Compatible |
| BSD-2-Clause | Apache-2.0 | ✅ Compatible |
| BSD-3-Clause | Apache-2.0 | ✅ Compatible |
| ISC | Apache-2.0 | ✅ Compatible |
| LGPL-2.1 | Apache-2.0 | ⚠️ Compatible* |
| LGPL-3.0 | Apache-2.0 | ⚠️ Compatible* |
| MPL-2.0 | Apache-2.0 | ⚠️ Compatible* |
| GPL-2.0 | Apache-2.0 | ❌ Incompatible |
| GPL-3.0 | Apache-2.0 | ❌ Incompatible |
| AGPL-3.0 | Apache-2.0 | ❌ Incompatible |

\* Requires manual review and compliance with license terms

## Notes

- All dependencies must have licenses from the allowlist
- Weak copyleft licenses require review before use
- Denylist licenses must not be used under any circumstances
- License information should be documented in SBOM

## References

- **SBOM**: `compliance/sbom/SBOM.template.json`
- **Compliance Documentation**: `docs/COMPLIANCE_GOVERNANCE.md`
- **Apache License 2.0**: https://www.apache.org/licenses/LICENSE-2.0

