(function() {
    const CATEGORY_COLUMNS = window.columnOrder.CATEGORY_COLUMNS;

    let currentOrder = [...CATEGORY_COLUMNS, 'image', 'color', 'quantity', 'name', 'partNumber'];
    let draggedColId = null;
    let draggedColType = null;
    let draggedElement = null;
    let draggedColumnIndex = null;

    document.addEventListener('DOMContentLoaded', init);

    function init() {
        initDesktopDragDrop();
        initTouchDragDrop();
        document.getElementById('resetColumnsBtn')?.addEventListener('click', resetColumnOrder);

        document.getElementById('inputFile').addEventListener('change', function() {
            if (this.files.length > 0) {
                document.getElementById('setNumber').value = '';
                document.getElementById('uploadForm').submit();
            }
        });
        document.getElementById('setNumber').addEventListener('keydown', function(e) {
            if (e.key === 'Enter' && this.value.trim() !== '') {
                document.getElementById('inputFile').value = '';
                document.getElementById('uploadForm').submit();
            }
        });
    }

    function initDesktopDragDrop() {
        document.querySelectorAll('th[draggable="true"]').forEach(th => {
            th.addEventListener('dragstart', handleDragStart);
            th.addEventListener('dragend', handleDragEnd);
            th.addEventListener('dragover', handleDragOver);
            th.addEventListener('dragleave', handleDragLeave);
            th.addEventListener('drop', handleDrop);
        });
    }

    function initTouchDragDrop() {
        document.querySelectorAll('th[draggable="true"]').forEach(th => {
            th.addEventListener('touchstart', handleTouchStart, { passive: false });
            th.addEventListener('touchmove', handleTouchMove, { passive: false });
            th.addEventListener('touchend', handleTouchEnd, { passive: false });
        });
    }

    function getCategoryRange() {
        const indices = currentOrder
            .map((col, idx) => CATEGORY_COLUMNS.includes(col) ? idx : -1)
            .filter(idx => idx >= 0);
        if (indices.length === 0) return { start: -1, end: -1 };
        return { start: indices[0], end: indices[indices.length - 1] };
    }

    function computeDropTarget(x) {
        const ths = document.querySelectorAll('th[data-col-id]');
        const range = getCategoryRange();
        const isDraggingCategory = draggedColType === 'category';

        let draggedIndices;
        if (isDraggingCategory) {
            draggedIndices = [];
            for (let i = 0; i < currentOrder.length; i++) {
                if (CATEGORY_COLUMNS.includes(currentOrder[i])) {
                    draggedIndices.push(i);
                }
            }
        } else {
            draggedIndices = [draggedColumnIndex];
        }

        const minDragged = Math.min(...draggedIndices);
        const maxDragged = Math.max(...draggedIndices);

        for (let i = 0; i < ths.length; i++) {
            const th = ths[i];
            const rect = th.getBoundingClientRect();
            const colIndex = currentOrder.indexOf(th.dataset.colId);
            const midX = rect.left + rect.width / 2;

            if (x < rect.right) {
                let targetEdge;
                if (x < midX) {
                    targetEdge = 'left';
                } else {
                    targetEdge = 'right';
                }

                // Apply category constraints
                const isInFirstTwoCategories = colIndex >= range.start && colIndex <= range.start + 1;
                const isInLastTwoCategories = colIndex >= range.end - 1 && colIndex <= range.end;

                if (isDraggingCategory) {
                    if (isInFirstTwoCategories) {
                        targetEdge = 'left';
                    } else if (isInLastTwoCategories) {
                        targetEdge = 'right';
                    }
                } else {
                    if (colIndex >= range.start && colIndex <= range.end) {
                        targetEdge = range.start === 0 ? 'left' : 'right';
                    }
                }

                return { columnIndex: colIndex, edge: targetEdge };
            }
        }

        // Past the last column - target is end of table
        return { columnIndex: currentOrder.length - 1, edge: 'end' };
    }

    function computeDropPosition(target) {
        const range = getCategoryRange();
        const isDraggingCategory = draggedColType === 'category';

        let draggedIndices;
        if (isDraggingCategory) {
            draggedIndices = [];
            for (let i = 0; i < currentOrder.length; i++) {
                if (CATEGORY_COLUMNS.includes(currentOrder[i])) {
                    draggedIndices.push(i);
                }
            }
        } else {
            draggedIndices = [draggedColumnIndex];
        }

        const minDragged = Math.min(...draggedIndices);
        const maxDragged = Math.max(...draggedIndices);

        let dropPosition;

        if (target.edge === 'end') {
            dropPosition = currentOrder.length;
        } else if (target.edge === 'left') {
            if (isDraggingCategory) {
                // For category drag: clamp to outside category range
                if (target.columnIndex <= range.start) {
                    dropPosition = range.start;
                } else {
                    dropPosition = range.end + 1;
                }
            } else {
                // For non-category: if within category range, move to after categories
                if (target.columnIndex >= range.start && target.columnIndex <= range.end) {
                    dropPosition = range.end + 1;
                } else {
                    dropPosition = target.columnIndex;
                }
            }
        } else { // target.edge === 'right'
            if (isDraggingCategory) {
                if (target.columnIndex < range.start) {
                    dropPosition = range.start;
                } else if (target.columnIndex >= range.end) {
                    dropPosition = range.end + 1;
                } else {
                    dropPosition = range.end + 1;
                }
            } else {
                if (target.columnIndex >= range.start && target.columnIndex <= range.end) {
                    dropPosition = range.end + 1;
                } else {
                    dropPosition = target.columnIndex + 1;
                }
            }
        }

        // Check if bordering on source - if so, highlight source (no move)
        const beforeSource = dropPosition === minDragged;
        const afterSource = dropPosition === maxDragged + 1;
        
        if (beforeSource || afterSource) {
            return { dropPosition: minDragged, highlightType: 'box' };
        }

        return { dropPosition: dropPosition, highlightType: 'line' };
    }

    function handleDragStart(e) {
        draggedColId = e.target.dataset.colId;
        draggedColType = e.target.dataset.colType;
        draggedElement = e.target;
        draggedColumnIndex = currentOrder.indexOf(draggedColId);
        e.target.classList.add('dragging');
        e.dataTransfer.effectAllowed = 'move';
    }

    function handleTouchStart(e) {
        const touch = e.touches[0];
        draggedElement = e.target.closest('th[draggable="true"]');
        if (draggedElement) {
            draggedColId = draggedElement.dataset.colId;
            draggedColType = draggedElement.dataset.colType;
            draggedColumnIndex = currentOrder.indexOf(draggedColId);
            draggedElement.classList.add('dragging');
        }
    }

    function handleDragEnd(e) {
        e.target.classList.remove('dragging');
        document.querySelectorAll('.drag-over').forEach(el => el.classList.remove('drag-over'));
        removeDropIndicator();

        applyDrop();

        draggedColId = null;
        draggedColType = null;
        draggedElement = null;
        draggedColumnIndex = null;
    }

    function handleDragOver(e) {
        e.preventDefault();
        e.dataTransfer.dropEffect = 'move';

        const x = e.clientX;
        updateDropIndicator(x);
    }

    function handleDragLeave(e) {
        const th = e.target.closest('th[data-col-id]');
        if (th) th.classList.remove('drag-over');
    }

    function handleDrop(e) {
        e.preventDefault();
        applyDrop();
    }

    function handleTouchMove(e) {
        if (!draggedElement) return;
        e.preventDefault();
        const touch = e.touches[0];
        document.querySelectorAll('.drag-over').forEach(el => el.classList.remove('drag-over'));

        const x = touch.clientX;
        updateDropIndicator(x);
    }

    function handleTouchEnd(e) {
        if (!draggedElement) return;
        e.preventDefault();

        applyDrop();

        draggedElement.classList.remove('dragging');
        document.querySelectorAll('.drag-over').forEach(el => el.classList.remove('drag-over'));
        removeDropIndicator();

        draggedColId = null;
        draggedColType = null;
        draggedElement = null;
        draggedColumnIndex = null;
    }

    let currentDropInfo = null;

    function updateDropIndicator(x) {
        const target = computeDropTarget(x);
        const dropInfo = computeDropPosition(target);
        
        if (currentDropInfo && 
            currentDropInfo.dropPosition === dropInfo.dropPosition && 
            currentDropInfo.highlightType === dropInfo.highlightType) {
            return;
        }
        
        currentDropInfo = dropInfo;
        showDropIndicator(dropInfo, target);
    }

    function showDropIndicator(dropInfo, target) {
        removeDropIndicator();

        const ths = document.querySelectorAll('th[data-col-id]');

        if (dropInfo.highlightType === 'box') {
            const indicator = document.createElement('div');
            indicator.className = 'drop-indicator-box';
            const th = ths[draggedColumnIndex];
            const rect = th.getBoundingClientRect();
            indicator.style.left = rect.left + 'px';
            indicator.style.width = rect.width + 'px';
            document.body.appendChild(indicator);
        } else {
            const indicator = document.createElement('div');
            indicator.className = 'drop-indicator-line';
            
            let leftPos;
            if (target.edge === 'end') {
                const lastTh = ths[ths.length - 1];
                const rect = lastTh.getBoundingClientRect();
                leftPos = rect.right;
            } else if (target.edge === 'left') {
                const th = ths[target.columnIndex];
                const rect = th.getBoundingClientRect();
                leftPos = rect.left;
            } else {
                const th = ths[target.columnIndex];
                const rect = th.getBoundingClientRect();
                leftPos = rect.right;
            }
            
            indicator.style.left = leftPos + 'px';
            document.body.appendChild(indicator);
        }
    }

    function removeDropIndicator() {
        document.querySelectorAll('.drop-indicator-box, .drop-indicator-line').forEach(el => el.remove());
    }

    function applyDrop() {
        if (!currentDropInfo || currentDropInfo.highlightType === 'box') return;

        const isDraggingCategory = draggedColType === 'category';
        
        let draggedIndices;
        if (isDraggingCategory) {
            draggedIndices = [];
            for (let i = 0; i < currentOrder.length; i++) {
                if (CATEGORY_COLUMNS.includes(currentOrder[i])) {
                    draggedIndices.push(i);
                }
            }
        } else {
            draggedIndices = [draggedColumnIndex];
        }

        const newOrder = window.columnOrder.moveToDropPosition(
            currentOrder,
            draggedColumnIndex,
            isDraggingCategory,
            currentDropInfo.dropPosition
        );

        if (newOrder.join(',') !== currentOrder.join(',')) {
            currentOrder = newOrder;
            reorderTable();
        }

        currentDropInfo = null;
    }

    function reorderTable() {
        const table = document.querySelector('table');
        if (!table) return;

        const headerCells = Array.from(table.querySelectorAll('thead th'));
        headerCells.sort((a, b) =>
            currentOrder.indexOf(a.dataset.colId) - currentOrder.indexOf(b.dataset.colId)
        );
        const headerRow = table.querySelector('thead tr');
        headerCells.forEach(cell => headerRow.appendChild(cell));

        table.querySelectorAll('tbody tr').forEach(row => {
            const cells = Array.from(row.querySelectorAll('td'));
            cells.sort((a, b) =>
                currentOrder.indexOf(a.dataset.colId) - currentOrder.indexOf(b.dataset.colId)
            );
            cells.forEach(cell => row.appendChild(cell));
        });
    }

    function resetColumnOrder() {
        currentOrder = window.columnOrder.resetColumns();
        reorderTable();
    }

    window.resetColumnOrder = resetColumnOrder;
})();
