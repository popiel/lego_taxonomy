(function() {
    const CATEGORY_COLUMNS = ['category', 'category2', 'category3', 'category4'];
    const CATEGORY_START = 0;
    const CATEGORY_END = 3;

    let currentOrder = [...CATEGORY_COLUMNS, 'image', 'color', 'quantity', 'name', 'partNumber'];
    let draggedColId = null;
    let draggedColType = null;
    let draggedElement = null;

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

    function handleDragStart(e) {
        draggedColId = e.target.dataset.colId;
        draggedColType = e.target.dataset.colType;
        draggedElement = e.target;
        e.target.classList.add('dragging');
        e.dataTransfer.effectAllowed = 'move';
    }

    function handleTouchStart(e) {
        const touch = e.touches[0];
        draggedElement = e.target.closest('th[draggable="true"]');
        if (draggedElement) {
            draggedColId = draggedElement.dataset.colId;
            draggedColType = draggedElement.dataset.colType;
            draggedElement.classList.add('dragging');
        }
    }

    function handleDragEnd(e) {
        e.target.classList.remove('dragging');
        document.querySelectorAll('.drag-over').forEach(el => el.classList.remove('drag-over'));
        removeDropIndicator();
        draggedColId = null;
        draggedColType = null;
        draggedElement = null;
    }

    function handleDragOver(e) {
        e.preventDefault();
        const th = e.target.closest('th[data-col-id]');
        if (!th || !canDropInPosition(th)) {
            removeDropIndicator();
            return;
        }
        e.dataTransfer.dropEffect = 'move';
        showDropIndicator(th, e.clientX);
    }

    function handleDragLeave(e) {
        const th = e.target.closest('th[data-col-id]');
        if (th) th.classList.remove('drag-over');
        removeDropIndicator();
    }

    function handleDrop(e) {
        e.preventDefault();
        const th = e.target.closest('th[data-col-id]');
        if (th) handleDropTarget(th);
    }

    function handleTouchMove(e) {
        if (!draggedElement) return;
        e.preventDefault();
        const touch = e.touches[0];
        const elementBelow = document.elementFromPoint(touch.clientX, touch.clientY);
        document.querySelectorAll('.drag-over').forEach(el => el.classList.remove('drag-over'));
        const th = elementBelow?.closest('th[data-col-id]');
        if (th && canDropInPosition(th)) {
            th.classList.add('drag-over');
            showDropIndicator(th, touch.clientX);
        } else {
            removeDropIndicator();
        }
    }

    function handleTouchEnd(e) {
        if (!draggedElement) return;
        e.preventDefault();
        const elementBelow = document.elementFromPoint(
            e.changedTouches[0].clientX,
            e.changedTouches[0].clientY
        );
        const th = elementBelow?.closest('th[data-col-id]');
        if (th) handleDropTarget(th);
        draggedElement.classList.remove('dragging');
        document.querySelectorAll('.drag-over').forEach(el => el.classList.remove('drag-over'));
        removeDropIndicator();
        draggedColId = null;
        draggedColType = null;
        draggedElement = null;
    }

    function showDropIndicator(th, clientX) {
        removeDropIndicator();
        const rect = th.getBoundingClientRect();
        const isAfter = clientX > rect.left + rect.width / 2;
        const indicator = document.createElement('div');
        indicator.className = 'drop-indicator ' + (isAfter ? 'after' : 'before');
        th.style.position = 'relative';
        th.appendChild(indicator);
    }

    function removeDropIndicator() {
        document.querySelectorAll('.drop-indicator').forEach(el => el.remove());
    }

    function handleDropTarget(th) {
        th.classList.remove('drag-over');
        const targetColId = th.dataset.colId;
        const fromIndex = currentOrder.indexOf(draggedColId);
        let toIndex = currentOrder.indexOf(targetColId);
        toIndex = constrainTarget(toIndex, draggedColType !== 'category');
        if (fromIndex !== toIndex) {
            moveColumnInOrder(fromIndex, toIndex);
            reorderTable();
        }
    }

    function canDropInPosition(th) {
        const targetIndex = currentOrder.indexOf(th.dataset.colId);
        if (draggedColType === 'category') {
            return targetIndex > CATEGORY_END;
        }
        return targetIndex < CATEGORY_START || targetIndex > CATEGORY_END;
    }

    function constrainTarget(targetIndex, isNonCategory) {
        if (isNonCategory) {
            if (targetIndex >= CATEGORY_START && targetIndex <= CATEGORY_END) {
                return CATEGORY_END + 1;
            }
        }
        return targetIndex;
    }

    function moveColumnInOrder(fromIndex, toIndex) {
        if (draggedColType === 'category') {
            moveCategoryGroupInOrder(fromIndex, toIndex);
        } else {
            const [moved] = currentOrder.splice(fromIndex, 1);
            const insertIndex = toIndex > fromIndex && toIndex > CATEGORY_END + 1 ? toIndex - 1 : toIndex;
            currentOrder.splice(insertIndex, 0, moved);
        }
    }

    function moveCategoryGroupInOrder(fromIndex, toIndex) {
        const firstCategoryIndex = currentOrder.findIndex(col => CATEGORY_COLUMNS.includes(col));
        const categoryCount = CATEGORY_COLUMNS.length;
        
        const targetIsInCategoryRange = toIndex <= CATEGORY_END;
        if (targetIsInCategoryRange) {
            return;
        }
        
        let adjustedToIndex = toIndex;
        if (toIndex > fromIndex) {
            adjustedToIndex = toIndex - categoryCount;
        } else {
            adjustedToIndex = Math.max(toIndex - categoryCount + 1, CATEGORY_END + 1);
        }
        
        const removedCategories = currentOrder.splice(firstCategoryIndex, categoryCount);
        currentOrder.splice(adjustedToIndex, 0, ...removedCategories);
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
        currentOrder = [...CATEGORY_COLUMNS, 'image', 'color', 'quantity', 'name', 'partNumber'];
        reorderTable();
    }

    window.resetColumnOrder = resetColumnOrder;
})();