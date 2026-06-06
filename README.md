# 🧰 AutoCAD Lisp Tools Workspace

This workspace contains a collection of AutoLISP tools for AutoCAD, organized by development stage.

## 📁 Workspace Structure (Non-Ignored)

### Main folders
- `Finished/`
- `developing/`
- `developing 2/`
- `Functions/`
- `lib/`
- `LISPS imported/`
- `Load/`
- `Projects/`

## ✅ Finished Tools

- <span style="font-size: 1.2em; font-weight: 800;"><strong>`BreakAtDist_BD.lsp`</strong></span>: Splits lines, polylines, or similar objects at user-defined distances from a picked start point.
	Example: APPLOAD the file, run `BD`, pick a line/polyline, and enter break distances.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`CopyAttsInc_CA.lsp`</strong></span>: Copies selected block attribute values to other blocks and can auto-increment chosen fields.
	Example: Run `CA`, pick source block, select destination blocks, and increment tags like 101, 102, 103.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`CopyAttsInc_CA old with round error.lsp`</strong></span>: Legacy attribute copy/increment version kept for compatibility and rounding troubleshooting.
	Example: Use in old projects to reproduce previous rounded outputs.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`Del_Dims_In_Selection.lsp`</strong></span>: Removes all dimension entities inside a selected window or selection set.
	Example: Window-select a crowded area and delete dimensions before re-dimensioning.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`DesignParkLot_DP.lsp`</strong></span>: Supports parking layout drafting by placing or organizing parking-related geometry with repeated rules.
	Example: Run on a site layout, set spacing/angle rules, and generate parking elements.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`Draw_Slope_line_DS.lsp`</strong></span>: Draws slope indicator lines between points with slope-related annotation logic.
	Example: Pick start/end points, set slope format, and place slope line annotations.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`EditNestedBlock_ENB.lsp`</strong></span>: Targets and edits objects nested inside block references without exploding blocks.
	Example: Select a nested object in a block and modify it directly.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`FilterSelection_Type_FS.lsp`</strong></span>: Filters selections by entity type so downstream commands affect only relevant objects.
	Example: Preselect mixed objects, choose TEXT, and keep only text selected.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`FindDuplicatedText.lsp`</strong></span>: Scans text/mtext to find duplicate content and highlight repeated labels.
	Example: Run on a plan sheet and review all duplicated notes.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`FindNumberJumps.lsp`</strong></span>: Reviews numeric text/attribute sequences and reports missing or irregular increments.
	Example: Select numbered labels and detect missing numbers like 17 between 16 and 18.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`HatchToPlineCheck_HTP.lsp`</strong></span>: Validates hatch boundaries against polylines and assists hatch-to-boundary checks.
	Example: Select hatch objects and verify related boundary polylines.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`RenameBlocks_REN.lsp`</strong></span>: Batch-renames block definitions using pattern rules while preserving references.
	Example: Apply a naming pattern such as `DOOR_*` to a selected block set.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`ReNumber_byIncreament_RNI.lsp`</strong></span>: Renumbers selected text/attributes by fixed increment and optional start value.
	Example: Set start 1 and increment 1 to resequence tags.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`ReNumber_byValue_RN.lsp`</strong></span>: Renumbers objects from explicit values/ranges when advanced sequence control is needed.
	Example: Enter target values and assign them to selected objects.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`RevCloud_auto_RV.lsp`</strong></span>: Creates revision clouds automatically around selected geometry or boundaries.
	Example: Select changed details and generate standardized revclouds.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`SelectNumbersInRange.lsp`</strong></span>: Selects text/attribute objects whose numeric value is within min/max limits.
	Example: Enter 200 to 300 and select all matching numeric labels.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`SelectSimilarProp.lsp`</strong></span>: Selects objects that match a source object's key properties.
	Example: Pick a reference entity and select all objects with same layer/color/linetype.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`SelTextByWord_STW.lsp`</strong></span>: Finds and selects text entities containing a target word or phrase.
	Example: Search for EXISTING and select all matching text/mtext.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`SetLayersColor.lsp`</strong></span>: Applies standardized colors to chosen layers in one operation.
	Example: Pick target layers and assign a color index in batch.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`Show_Block_Points.lsp`</strong></span>: Displays key block points (insertion/base/reference points) for alignment checks.
	Example: Select block references and show insertion points for QC.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`spot_slope_SL.lsp`</strong></span>: Places or updates spot slope labels from picked points/elevations.
	Example: Pick terrain points and generate formatted grading slope labels.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`UpdateAttsInc_UA.lsp`</strong></span>: Updates block attributes across references and can increment selected fields.
	Example: Select destination blocks, update attributes, and increment sequence tags.

## 🧪 Developing Tools

- <span style="font-size: 1.2em; font-weight: 800;"><strong>`AutoRotate_rr.lsp`</strong></span>: Automatically rotates selected objects to match nearby geometry direction, UCS context, or a chosen reference angle.
	Example: Select symbols, pick a reference edge, and auto-align orientation.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`ContourBuild.lsp`</strong></span>: Generates contour lines from source points/polylines and streamlines repetitive drafting.
	Example: Input survey data, set contour interval, and create contour polylines.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`CopyBlkPropsToDims.lsp`</strong></span>: Reads properties from a block and applies mapped settings to dimension objects.
	Example: Pick source block style and apply it to selected dimensions.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`Count rectangle dims_RecDims.LSP`</strong></span>: Detects rectangular dimension patterns, counts them, and reports statistics.
	Example: Run on a detail area to produce quantity/size count summaries.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`DesingGroup_ParkingLots_DPG.lsp`</strong></span>: Assists drafting and grouping parking lot components into organized sets.
	Example: Group drafted stalls for easier editing and scheduling.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`draw_slope_options.lsp`</strong></span>: Provides configurable slope drawing modes before creating slope graphics.
	Example: Choose ratio or percent style and draw slope annotations.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`DrawClothoid.lsp`</strong></span>: Draws transition clothoid curves from geometric inputs.
	Example: Enter tangent/curve parameters and generate a transition alignment.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`DrawClothoid_allMethods.lsp`</strong></span>: Implements multiple clothoid construction methods for different constraints.
	Example: Select a method based on known inputs and build the curve.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`DrawClothoid_tst_equations.lsp`</strong></span>: Testing version used to validate clothoid equations and output geometry.
	Example: Compare generated curves against expected test values.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`DrawClothoid2.lsp`</strong></span>: Alternate clothoid implementation with different logic/user flow.
	Example: Use when the first clothoid method is not suitable for given inputs.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`extract_block_door_window.lsp`</strong></span>: Extracts door/window block names, counts, attributes, and dimensions for reporting.
	Example: Select architectural blocks and export schedule-ready data.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`Filter_By_Used_Color.lsp`</strong></span>: Filters/selects entities by colors used in the drawing for standards cleanup.
	Example: Isolate all objects using a chosen color index.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`FindNearByVertice.lsp`</strong></span>: Finds vertices near a picked point within a tolerance to detect small gaps/issues.
	Example: Pick near junctions to locate close polyline vertices.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`FixBlockBase.lsp`</strong></span>: Repositions or normalizes block base points for predictable insertion behavior.
	Example: Redefine a misaligned block base point and verify placement.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`FreezeDynAtt_NotWorking.lsp`</strong></span>: Experimental script to freeze/lock dynamic attribute outcomes; currently unstable.
	Example: Test in a sandbox file before any production use.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`GoToLayer.lsp`</strong></span>: Quickly isolates, activates, or zooms to content on a specified layer.
	Example: Enter a layer name and jump directly to its objects.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`grid_contour.lsp`</strong></span>: Builds grid-based contour helpers for terrain drafting/interpolation checks.
	Example: Generate grid references and contour guidance from gridded points.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`grread-tst.lsp`</strong></span>: Test utility for keyboard/mouse event capture with grread.
	Example: Run and interact to validate custom command input handling.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`IncText_NM.lsp`</strong></span>: Increments numeric text labels in selection order for controlled numbering.
	Example: Select labels, set start/step, and auto-number sequence.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`interactive_UI_commands.lsp`</strong></span>: Playground for testing custom prompts and interactive command behavior.
	Example: Run prototypes and validate selection/prompt UX.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`ModifyBlockAndNested_MB.lsp`</strong></span>: Applies changes to blocks and nested elements in one workflow.
	Example: Select block references and update nested properties without deep manual edits.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`MultiExportDWG.lsp`</strong></span>: Batch-exports selected layouts/blocks/areas to multiple DWG files.
	Example: Choose output folder and export multiple drawings in one run.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`OverKillFromBlock.lsp`</strong></span>: Removes duplicate/overlapping geometry inside block definitions to reduce clutter.
	Example: Clean a block definition before publishing the drawing set.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`SelectCoveringPlines.lsp`</strong></span>: Finds polylines that cover or contain target objects for boundary-based processing.
	Example: Select target objects and return enclosing polylines.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`Set_LTS100_Inblocks.lsp`</strong></span>: Sets linetype scale to 100 for entities inside blocks.
	Example: Apply to selected blocks to standardize displayed linetypes.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`Set_props_FromObj.lsp`</strong></span>: Copies properties from a source entity and applies them in bulk to targets.
	Example: Pick source object once and apply to multiple destinations.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`Set_props_FromObj old.lsp`</strong></span>: Older property-transfer version kept for fallback/compatibility.
	Example: Use when reproducing behavior from previous projects.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`TaArc-functions.lsp`</strong></span>: Shared helper functions for TaArc-related scripts and arc calculations.
	Example: Load first, then run dependent TaArc commands.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`trim_within_boundary.lsp`</strong></span>: Trims selected entities so only geometry inside a boundary remains.
	Example: Pick closed boundary and clean geometry outside it.

## 🧱 Developing 2 Tools

- <span style="font-size: 1.2em; font-weight: 800;"><strong>`Add_Attr_To_Block.lsp`</strong></span>: Adds new attribute definitions to existing block definitions and updates block references.
	Example: Select block definition, define tag/prompt/default, and push attributes to references.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`get_Reinf_info_RI.lsp`</strong></span>: Collects reinforcement information from drawing objects and outputs structured tabulation data.
	Example: Select rebar objects and extract counts/attributes for checking tables.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`insert_table_sections.lsp`</strong></span>: Inserts and formats section tables based on selected drawing data.
	Example: Pick insertion point and generate section table from current selection.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`LReinf.lsp`</strong></span>: Reinforcement drafting utility for rebar-related geometry and annotation.
	Example: Place and edit rebar marks/details in reinforcement drawings.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`Pline vertix7.lsp`</strong></span>: Provides polyline vertex reading/editing/processing operations for correction workflows.
	Example: Select polyline, modify target vertices, and update geometry.
- <span style="font-size: 1.2em; font-weight: 800;"><strong>`set_objs_props.lsp`</strong></span>: Applies chosen object properties to a target selection in batch.
	Example: Select desired layer/color/linetype settings and apply to many objects at once.




